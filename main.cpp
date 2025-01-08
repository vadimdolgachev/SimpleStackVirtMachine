#include <cassert>
#include <charconv>
#include <complex>
#include <cstdint>
#include <cstring>
#include <format>
#include <fstream>
#include <iostream>
#include <iterator>
#include <memory>
#include <sstream>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

enum OpCode : uint8_t {
    OpPush = 0x10,
    OpPop = 0x11,
    OpInc = 0x30,
    OpDec = 0x31,
    OpAdd = 0x32,
    OpSub = 0x33,
    OpMul = 0x34,
    OpDiv = 0x35,
    OpSqrt = 0x36,
    OpFtoi = 0x37,
    OpJmp = 0x50,
    OpJg = 0x51,
    OpStore = 0x52,
    OpLoad = 0x53,
    OpCall = 0x54,
    OpHalt = 0x55,
    OpLabel = 0x71
};

struct VM;

void executePush(VM &, OpCode);
void executePop(VM &, OpCode);
void executeIncDec(VM &, OpCode);
void executeAddSub(VM &, OpCode);
void executeMulDiv(VM &, OpCode);
void executeSqrt(VM &, OpCode);
void executeFtoi(VM &, OpCode);
void executeJmp(VM &, OpCode);
void executeJg(VM &, OpCode);
void executeStore(VM &, OpCode);
void executeLoad(VM &, OpCode);
void executeCall(VM &, OpCode);
void executeHalt(VM &, OpCode);

const std::unordered_map<OpCode, std::tuple<std::string, void(*)(VM &, OpCode)>> instructions = {
    {OpPush, std::make_tuple("push", executePush)},
    {OpPop, std::make_tuple("pop", executePop)},
    {OpInc, std::make_tuple("inc", executeIncDec)},
    {OpDec, std::make_tuple("dec", executeIncDec)},
    {OpAdd, std::make_tuple("add", executeAddSub)},
    {OpSub, std::make_tuple("sub", executeAddSub)},
    {OpMul, std::make_tuple("mul", executeMulDiv)},
    {OpDiv, std::make_tuple("div", executeMulDiv)},
    {OpSqrt, std::make_tuple("sqrt", executeSqrt)},
    {OpFtoi, std::make_tuple("ftoi", executeFtoi)},
    {OpJmp, std::make_tuple("jmp", executeJmp)},
    {OpJg, std::make_tuple("jg", executeJg)},
    {OpStore, std::make_tuple("store", executeStore)},
    {OpLoad, std::make_tuple("load", executeLoad)},
    {OpCall, std::make_tuple("call", executeCall)},
    {OpHalt, std::make_tuple("halt", executeHalt)},
};

class Stack final {
public:
    explicit Stack(const size_t capacity) :
        mem(capacity, 0xFF) {

    }

    void push(const int32_t val) {
        mem[index++] = val;
    }

    [[nodiscard]] int32_t top() const {
        return mem[index - 1];
    }

    void pop() {
        // only for debug
        mem[index - 1] = 0xFF;
        --index;
    }

    [[nodiscard]] int32_t left() const {
        return static_cast<int32_t>(mem.size()) - count();
    }

    [[nodiscard]] int32_t count() const {
        return index;
    }

    int32_t operator[](const size_t index) const {
        return mem[index];
    }

    int32_t &operator[](const size_t index) {
        return mem[index];
    }

private:
    std::vector<int32_t> mem;
    int32_t index = 0;
};

struct VM final {
    explicit VM(std::vector<uint8_t> memory, const size_t stackSize = 1024):
        mem(std::move(memory)),
        stack(stackSize) {

    }

    VM(const VM &) = delete;
    VM &operator=(const VM &vm) = delete;

    std::vector<uint8_t> mem;
    Stack stack;
    uint32_t pc = 0;
};

class BinaryStream final : public std::istream {
    struct BinaryBuf final : std::streambuf {
        BinaryBuf(uint8_t *const begin, uint8_t *const end) {
            setg(reinterpret_cast<char *>(begin),
                 reinterpret_cast<char *>(begin),
                 reinterpret_cast<char *>(end));
        }
    };

public:
    BinaryStream(uint8_t *const begin, uint8_t *const end) :
        buffer(begin, end) {
        rdbuf(&buffer);
    }

private:
    BinaryBuf buffer;
};

class BinaryWriter final {
public:
    explicit BinaryWriter(std::ostream &stream):
        stream(stream) {
    }

    BinaryWriter(BinaryWriter &) = delete;
    BinaryWriter &operator=(BinaryWriter &) = delete;

    void writeByte(const std::uint8_t value) {
        stream.put(static_cast<char>(value));
    }

    void writeInt32(const int32_t value) {
        writeByte((value & 0xFF000000) >> 24);
        writeByte((value & 0xFF0000) >> 16);
        writeByte((value & 0xFF00) >> 8);
        writeByte(value & 0xFF);
    }

private:
    std::ostream &stream;
};

class BinaryReader final {
public:
    explicit BinaryReader(std::istream &stream):
        stream(stream) {
    }

    BinaryReader(BinaryWriter &) = delete;
    BinaryReader &operator=(BinaryReader &) = delete;

    uint8_t readByte() {
        return stream.get();
    }

    int32_t readInt32() {
        return readByte() << 24 | readByte() << 16 | readByte() << 8 | readByte();
    }

private:
    std::istream &stream;
};

struct Instruction final {
    OpCode opcode;
    using OperandType = std::variant<int32_t, std::string>;
    using OperandsType = std::array<OperandType, 2>;
    OperandsType operands = {};
};

struct SourceFile final {
    std::vector<Instruction> instructions;
    using LabelTable = std::unordered_map<std::string, int32_t>;
    LabelTable labels;
};

std::string_view trim(const std::string_view text) {
    const auto start = text.find_first_not_of(' ');
    const auto end = text.find_last_not_of(' ');
    if (start == std::string_view::npos || end == std::string_view::npos) {
        return {};
    }
    return {text.data() + start, end - start + 1};
}

std::vector<std::string> split(const std::string_view s, const char delim) {
    std::vector<std::string> elems;
    std::istringstream iss(s.data());
    std::string item;
    while (std::getline(iss, item, delim)) {
        elems.push_back(item);
    }
    return elems;
}

template<class... Ts>
struct overloaded : Ts... {
    using Ts::operator()...;
};

Instruction::OperandType parseOperand(const std::string &operand) {
    if (int32_t value = 0;
        std::from_chars(operand.data(),
                        operand.data() + operand.size(),
                        value).ec == std::errc()) {
        return value;
    }
    return operand;
}


void compilePush(const Instruction::OperandsType &args,
                 const SourceFile::LabelTable &labels,
                 BinaryWriter &writer) {
    int32_t pos = 0;
    std::visit(
        overloaded{
            [&pos](const int32_t v) {
                pos = v;
            },
            [&labels, &pos](const std::string &label) {
                if (const auto p = labels.find(label); p != labels.end()) {
                    pos = p->second;
                } else {
                    throw std::logic_error(std::format("Label '{}' not found", label));
                }
            }
        },
        args[0]);
    writer.writeInt32(pos);
}

void compile(const SourceFile &file, BinaryWriter &writer) {
    for (const auto &[opcode, args] : file.instructions) {
        if (opcode == OpLabel) {
            continue;
        }
        writer.writeByte(opcode);
        if (opcode == OpPush) {
            compilePush(args, file.labels, writer);
        }
    }
}

void executePush(VM &vm, const OpCode) {
    if (vm.stack.left() == 0) {
        throw std::logic_error("Stack overflow");
    }
    BinaryStream stream(&vm.mem[vm.pc], &vm.mem[vm.pc + 4]);
    BinaryReader reader(stream);
    vm.stack.push(reader.readInt32());
    vm.pc += 4;
}

void executePop(VM &vm, const OpCode) {
    if (vm.stack.count() == 0) {
        throw std::logic_error("Stack underflow");
    }
    vm.stack.pop();
}

void executeIncDec(VM &vm, const OpCode opcode) {
    if (vm.stack.count() == 0) {
        throw std::logic_error("Stack underflow");
    }
    const auto value = vm.stack.top();
    vm.stack.pop();
    if (opcode == OpInc) {
        vm.stack.push(value + 1);
    } else if (opcode == OpDec) {
        vm.stack.push(value - 1);
    }
}

void executeAdd(VM &vm, const OpCode opcode) {
    if (vm.stack.count() < 2) {
        throw std::logic_error("Stack underflow");
    }
    const int32_t value1 = vm.stack.top();
    vm.stack.pop();
    const int32_t value2 = vm.stack.top();
    vm.stack.pop();
    if (opcode == OpAdd) {
        vm.stack.push(value1 + value2);
    } else if (opcode == OpSub) {
        vm.stack.push(value1 - value2);
    }
}

void executeAddSub(VM &vm, const OpCode opcode) {
    if (vm.stack.count() < 2) {
        throw std::logic_error("Stack underflow");
    }
    const int32_t value1 = vm.stack.top();
    vm.stack.pop();
    const int32_t value2 = vm.stack.top();
    vm.stack.pop();
    if (opcode == OpAdd) {
        vm.stack.push(value1 + value2);
    } else if (opcode == OpSub) {
        vm.stack.push(value1 - value2);
    }
}

void executeMulDiv(VM &vm, const OpCode opcode) {
    if (vm.stack.count() < 2) {
        throw std::logic_error("Stack underflow");
    }
    const auto value1 = vm.stack.top();
    vm.stack.pop();
    const auto value2 = vm.stack.top();
    vm.stack.pop();
    if (opcode == OpMul) {
        vm.stack.push(value1 * value2);
    } else if (opcode == OpDiv) {
        if (value2 == 0) {
            throw std::logic_error("Division by zero");
        }
        vm.stack.push(value1 / value2);
    }
}

void executeSqrt(VM &vm, const OpCode) {
    if (vm.stack.count() < 1) {
        throw std::logic_error("Stack underflow");
    }
    const auto value = vm.stack.top();
    vm.stack.pop();
    const auto sqrt = static_cast<float>(std::sqrt(value));
    int32_t internal = 0;
    memcpy(&internal, &sqrt, 4);
    vm.stack.push(internal);
}

void executeFtoi(VM &vm, const OpCode) {
    if (vm.stack.count() < 1) {
        throw std::logic_error("Stack underflow");
    }
    const auto value = vm.stack.top();
    vm.stack.pop();
    float floatValue = 0;
    memcpy(&floatValue, &value, 4);
    vm.stack.push(static_cast<int32_t>(floatValue));
}

void executeJmp(VM &vm, const OpCode) {
    if (vm.stack.count() == 0) {
        throw std::logic_error("Stack underflow");
    }
    const auto newPos = vm.stack.top();
    vm.stack.pop();
    if (newPos < 0 || static_cast<uint32_t>(newPos) >= vm.mem.size()) {
        throw std::logic_error("Invalid argument for jmp instruction");
    }
    vm.pc = newPos;
}

void executeJg(VM &vm, const OpCode) {
    if (vm.stack.count() < 3) {
        throw std::logic_error("Stack underflow");
    }
    const auto newPos = vm.stack.top();
    vm.stack.pop();
    if (newPos < 0 || static_cast<size_t>(newPos) >= vm.mem.size()) {
        throw std::logic_error("Invalid argument for jg operation");
    }
    const auto operand1 = vm.stack.top();
    vm.stack.pop();
    const auto operand2 = vm.stack.top();
    vm.stack.pop();
    if (operand2 > operand1) {
        vm.pc = newPos;
    }
}

void executeCall(VM &vm, const OpCode opcode) {
    const auto currPos = static_cast<int32_t>(vm.pc);
    executeJmp(vm, opcode);
    vm.stack.push(currPos);
}

void executeStore(VM &vm, const OpCode) {
    if (vm.stack.count() < 2) {
        throw std::logic_error("Stack underflow");
    }
    int32_t writePos = vm.stack.top();
    vm.stack.pop();
    int32_t readPos = vm.stack.top();
    vm.stack.pop();

    if (writePos < 0) {
        writePos = vm.stack.count() + writePos;
    }
    if (writePos < 0 || writePos >= vm.stack.count()) {
        throw std::logic_error("Invalid argument for stor instruction");
    }

    if (readPos < 0) {
        readPos = vm.stack.count() + readPos;
    }
    if (readPos < 0 || readPos >= vm.stack.count()) {
        throw std::logic_error("Invalid argument for stor instruction");
    }
    vm.stack[writePos] = vm.stack[readPos];
}

void executeLoad(VM &vm, const OpCode) {
    if (vm.stack.count() == 0) {
        throw std::logic_error("Stack underflow");
    }
    int32_t readPos = vm.stack.top();
    vm.stack.pop();

    if (readPos < 0) {
        readPos = vm.stack.count() + readPos;
    }
    if (readPos < 0 || readPos >= vm.stack.count()) {
        throw std::logic_error("Invalid argument for load instruction");
    }
    vm.stack.push(vm.stack[readPos]);
}

void executeHalt(VM &vm, const OpCode) {
    vm.pc = vm.mem.size();
}

Instruction parseInstruction(const std::string_view line) {
    const auto tokens = split(line, ' ');
    const auto &opName = tokens[0];
    if (opName == "label") {
        if (tokens.size() != 2) {
            throw std::runtime_error(std::format("Invalid instruction: {}", line));
        }
        return {OpLabel, {tokens[1]}};
    }
    const auto it = std::ranges::find_if(instructions, [&opName](const auto &arg) {
        return std::get<0>(arg.second) == opName;
    });
    if (it == instructions.end()) {
        throw std::logic_error("Invalid instruction");
    }
    if (it->first == OpPush) {
        if (tokens.size() != 2) {
            throw std::runtime_error(std::format("Invalid instruction: {}", line));
        }
        return {OpPush, {parseOperand(tokens[1])}};
    }
    return {it->first};
}

SourceFile parse(const std::string &text) {
    SourceFile sourceFile;
    int32_t pos = 0;
    std::istringstream iss(text);
    std::string line;
    uint32_t lineNumber = 1;
    while (std::getline(iss, line)) {
        try {
            const auto trimmed = trim(line);
            if (trimmed.empty() || trimmed.starts_with(';')) {
                continue;
            }
            const auto inst = parseInstruction(trimmed);
            const auto [opcode, args] = inst;
            sourceFile.instructions.push_back(inst);
            switch (opcode) {
            case OpPush:
                pos += sizeof(OpCode) + 4;
                break;
            case OpLabel:
                std::visit(
                    overloaded{
                        [](auto) {
                        },
                        [&sourceFile, pos](const std::string &label) {
                            sourceFile.labels[label] = pos;
                        }
                    },
                    args[0]);
                break;
            default:
                pos += sizeof(OpCode);
            }
            ++lineNumber;
        } catch (const std::exception &e) {
            throw std::logic_error(std::format("{} at line: {} | {}",
                                               e.what(), std::to_string(lineNumber), line));
        }
    }
    return sourceFile;
}

void execute(VM &vm) {
    while (vm.pc < vm.mem.size()) {
        const auto opcode = static_cast<OpCode>(vm.mem[vm.pc]);
        vm.pc += sizeof(OpCode);
        const auto instruction = instructions.find(opcode);
        if (instruction == instructions.end()) {
            throw std::logic_error("Invalid opcode");
        }
        const auto &[name, executor] = instruction->second;
        // std::cout << "op: " << name << "\n";
        executor(vm, opcode);
        // for (int i = 0; i < 20; ++i) {
        //    std::cout << std::format("{:02X} ", vm.stack[i]);
        // }
        // std::cout << "\n";
        // std::cout << vm.stack[0] << " " << vm.stack[1] << std::endl;
    }
}

void testSolveQuadraticEquation() {
    const std::string text = R"(
; return value
push 0
;a
push 1
;b
push -11
;c
push -152

push calc_D
call

; root x1
push 0
; root x2
push 0

push calc_root1
call

push calc_root2
call

halt

label calc_D
    ; a
    push -4
    load
    ; c
    push -3
    load

    ; 4*a*c
    mul
    push 4
    mul

    ; b
    push -4
    load

    ; b^2
    push -1
    load
    mul

    sub

    push -1
    push -6
    store
    pop
    jmp

label calc_root1
    ; D
    push -7
    load
    ; a
    push -7
    load
    ; b
    push -7
    load

    ; -b
    push -1
    mul

    ; sqrt(D)
    push -3
    load
    sqrt
    ftoi

    add

    ; 2 * a
    push -2
    load
    push 2
    mul

    push -2
    load

    div

    ; return
    push -1
    push -7
    store

    pop
    pop
    pop
    pop

    jmp

label calc_root2
    ; D
    push -7
    load
    ; a
    push -7
    load
    ; b
    push -7
    load

    ; -b
    push -1
    mul

    ; sqrt(D)
    push -3
    load
    sqrt
    ftoi

    push -1
    mul

    add

    ; 2 * a
    push -2
    load
    push 2
    mul

    push -2
    load

    div

    ; return
    push -1
    push -6
    store

    pop
    pop
    pop
    pop

    jmp
)";
    try {
        std::stringstream stream;
        BinaryWriter writer(stream);
        compile(parse(text), writer);
        VM vm({std::istreambuf_iterator(stream), std::istreambuf_iterator<char>()});
        execute(vm);
        if (vm.stack[4] != 19 || vm.stack[5] != -8) {
            throw std::logic_error(std::format("test failed: {} at {}", __FUNCTION__, __LINE__));
        }
    } catch (std::exception &e) {
        std::cout << e.what() << "\n";
        abort();
    }
}

void testSum1() {
    const std::string text = R"(
    push 2
    push 15
    push -1
    load
    inc
    mul
    div
)";
    try {
        std::stringstream stream;
        BinaryWriter writer(stream);
        compile(parse(text), writer);
        VM vm({std::istreambuf_iterator(stream), std::istreambuf_iterator<char>()});
        execute(vm);
        if (vm.stack[0] != 120) {
            throw std::logic_error(std::format("test failed: {} at {}", __FUNCTION__, __LINE__));
        }
    } catch (std::exception &e) {
        std::cout << e.what() << "\n";
        abort();
    }
}

void testSum2() {
    const std::string text = R"(
    ; counter
    push 1
    ; sum
    push 1

    label sum
        push 15
        push -3
        load
        push next_sum
        jg
        halt
        label next_sum
            push -2
            load
            inc
            push -1
            push -3
            store
            pop

            push -2
            load
            push -2
            load
            add
            push -1
            push -2
            store
            pop

            push sum
            jmp
)";
    try {
        std::stringstream stream;
        BinaryWriter writer(stream);
        compile(parse(text), writer);
        VM vm({std::istreambuf_iterator(stream), std::istreambuf_iterator<char>()});
        execute(vm);
        if (vm.stack[1] != 120) {
            throw std::logic_error(std::format("test failed: {} at {}", __FUNCTION__, __LINE__));
        }
    } catch (std::exception &e) {
        std::cout << e.what() << "\n";
        abort();
    }
}

int main() {
    testSolveQuadraticEquation();
    testSum1();
    testSum2();

    const std::string text = R"(
    ; counter
    push 1
    ; acc
    push 1

    label factorial
        push 5
        push -3
        load
        push next
        jg
        halt
        label next
            push -2
            load
            inc
            push -1
            push -3
            store
            pop

            push -2
            load
            push -2
            load
            mul
            push -1
            push -2
            store
            pop

            push factorial
            jmp
)";

    try {
        {
            std::ofstream file("out", std::ios::binary);
            BinaryWriter writer(file);
            compile(parse(text), writer);
        }
        std::ifstream file("out", std::ios::binary);
        VM vm({std::istreambuf_iterator(file), std::istreambuf_iterator<char>()});
        execute(vm);
        std::cout << "the factorial of 5 is " << vm.stack[1] << "\n";
    } catch (std::exception &e) {
        std::cout << e.what() << "\n";
    }
    return 0;
}
