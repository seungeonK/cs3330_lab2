// Seungeon Kim
// sk6qk
// 03/23/2021
// sim.c

#include <stdio.h>
#include "shell.h"
#include <stdlib.h>
#define OP_SPECIAL 0x00
#define SUBOP_ADD 0x20
#define SUBOP_ADDU 0x21
#define OP_ADDI 0x08
#define OP_ADDIU 0x09
#define SUBOP_SYSCALL 0xc

/*-------Part I---------*/
#define OP_ORI 0x0d
// Arithmetic & logical instruction
#define SUBOP_SUB 0x22
#define SUBOP_SUBU 0x23
#define SUBOP_SLL 0x00
#define SUBOP_SRL 0x02
#define SUBOP_SRA 0x03
#define SUBOP_MULTU 0x19
#define SUBOP_DIVU 0x1b
// Comparision Instruction
#define SUBOP_SLTU 0x29
// Data Movement Instruction
#define SUBOP_MFHI 0x10

/*-------Part II---------*/
// jump instruction
#define OP_J 0x02
// branch instructon
#define OP_BEQ 0x04
#define OP_BNE 0x05
#define OP_BGTZ 0x07
// load instructons
#define OP_LUI 0x0F
#define OP_LB 0x20
#define OP_LBU 0x24
#define OP_LW 0x23
// store instructions
#define OP_SB 0x28
#define OP_SW 0x2B

/*-------Bonus---------*/
#define OP_SLTI 0x0A
#define OP_SLTIU 0x0B
#define SUBOP_AND 0x24
#define SUBOP_OR 0x25
#define SUBOP_XOR 0x26
#define SUBOP_NOR 0x27
#define SUBOP_SLT 0x2A
#define SUBOP_MFLO 0x12
#define SUBOP_MTHI 0x11
#define SUBOP_DIV 0x1a
#define OP_BLEZ 0x06
#define OP_BGEZ 0x01
/*-------10 additional instructions---------*/


uint32_t dcd_op;     /* decoded opcode */
uint32_t dcd_rs;     /* decoded rs operand */
uint32_t dcd_rt;     /* decoded rt operand */
uint32_t dcd_rd;     /* decoded rd operand */
uint32_t dcd_shamt;  /* decoded shift amount */
uint32_t dcd_funct;  /* decoded function */
uint32_t dcd_imm;    /* decoded immediate value */
uint32_t dcd_target; /* decoded target address */
int dcd_se_imm;      /* decoded sign-extended immediate value */
uint32_t inst;       /* machine instruction */

int64_t mult_temp;
uint32_t virtualAddress;

uint32_t sign_extend_h2w(uint16_t c)
{
    return (c & 0x8000) ? (c | 0xffff8000) : c;
}

uint32_t sign_extend_b2w(uint8_t c)
{
    return (c & 0x80) ? (c | 0xffffff80) : c;
}
uint32_t zero_extend_b2w(uint8_t c)
{
    return ((uint32_t) c);
}
uint32_t zero_extend_h2w(uint16_t c)
{
    return ((uint32_t) c);
}
void fetch()
{
    /* fetch the 4 bytes of the current instruction */
    inst = mem_read_32(CURRENT_STATE.PC);
}

void decode()
{
    /* decoding an instruction */
    dcd_op = (inst >> 26) & 0x3F;
    dcd_rs = (inst >> 21) & 0x1F;
    dcd_rt = (inst >> 16) & 0x1F;
    dcd_rd = (inst >> 11) & 0x1F;
    dcd_shamt = (inst >> 6) & 0x1F;
    dcd_funct = (inst >> 0) & 0x3F;
    dcd_imm = (inst >> 0) & 0xFFFF;
    dcd_se_imm = sign_extend_h2w(dcd_imm);
    dcd_target = (inst >> 0) & ((1UL << 26) - 1);
}

void execute()
{
    //PC = PC+4
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
    CURRENT_STATE.REGS[0] = 0;

    switch (dcd_op)
    {
        case OP_SPECIAL:
            switch (dcd_funct){
                case SUBOP_ADD:
                    NEXT_STATE.REGS[dcd_rd]=(int)CURRENT_STATE.REGS[dcd_rs]+(int)CURRENT_STATE.REGS[dcd_rt];
                    break;
                case SUBOP_ADDU:
                    NEXT_STATE.REGS[dcd_rd] = CURRENT_STATE.REGS[dcd_rs] + CURRENT_STATE.REGS[dcd_rt];
                    break;
                case SUBOP_SYSCALL:
                    if (CURRENT_STATE.REGS[2] == 10)
                        RUN_BIT = 0;
                    break;
                /* ----------BONUS----------*/
                case SUBOP_AND:
                    NEXT_STATE.REGS[dcd_rd]= CURRENT_STATE.REGS[dcd_rs] & CURRENT_STATE.REGS[dcd_rt];
                    break;
                
                case SUBOP_OR:
                    NEXT_STATE.REGS[dcd_rd] = CURRENT_STATE.REGS[dcd_rs] | CURRENT_STATE.REGS[dcd_rt];
		            break;
                
                case SUBOP_XOR:
		            NEXT_STATE.REGS[dcd_rd] = CURRENT_STATE.REGS[dcd_rs] ^ CURRENT_STATE.REGS[dcd_rt];
		            break;

                case SUBOP_NOR:
		            NEXT_STATE.REGS[dcd_rd] = ~(CURRENT_STATE.REGS[dcd_rs] | CURRENT_STATE.REGS[dcd_rt]);
		            break;
                // set on less than
                case SUBOP_SLT:
                    if ((int32_t)CURRENT_STATE.REGS[dcd_rs]<(int32_t)CURRENT_STATE.REGS[dcd_rt])
                        NEXT_STATE.REGS[dcd_rd] = 1;
                    else
                        NEXT_STATE.REGS[dcd_rd] = 0;
                    break;
                // move from Lo
                // the contents of special register LO are loaded into general register rd
                // $d = LO
                case SUBOP_MFLO:
                    NEXT_STATE.REGS[dcd_rd] = CURRENT_STATE.LO;
                    break;
                // move to Hi
                // the contents of general register Rs are loaded into special register HI
                // HI = $s
                case SUBOP_MTHI:
                    NEXT_STATE.HI = CURRENT_STATE.REGS[dcd_rs];
                    break;
                // DIV rs, rt
                case SUBOP_DIV:
                    if (dcd_rt == 0){
                        fprintf(stderr, "can't divided by 0\n");
                        exit(-1);
                    }
                    // Operation
                    // hi = $s % $t
                    NEXT_STATE.HI=((int32_t)CURRENT_STATE.REGS[dcd_rs] % (int32_t)CURRENT_STATE.REGS[dcd_rt]);
                    // lo = $s / $t
			        NEXT_STATE.LO=((int32_t)CURRENT_STATE.REGS[dcd_rs] / (int32_t)CURRENT_STATE.REGS[dcd_rt]);
                    break;
                /* ----------BONUS END----------*/
                /* ----------PART I----------*/
                case SUBOP_SLL:
                //bitwise left-shift
                NEXT_STATE.REGS[dcd_rd]=CURRENT_STATE.REGS[dcd_rt] << dcd_shamt;
                break;
                // Subtract word. Rd(destination) = Rs(source) - Rt(target)
                case SUBOP_SUB:
                    NEXT_STATE.REGS[dcd_rd] = (int32_t)CURRENT_STATE.REGS[dcd_rs] - (int32_t)CURRENT_STATE.REGS[dcd_rt];
                    break;
                // Subtract unsigned word. Rd = Rs - Rt
                case SUBOP_SUBU:
                    NEXT_STATE.REGS[dcd_rd] = CURRENT_STATE.REGS[dcd_rs]-CURRENT_STATE.REGS[dcd_rt];
                    break;
                // Shift word right logical
                case SUBOP_SRL:
                    NEXT_STATE.REGS[dcd_rd]=(uint32_t)CURRENT_STATE.REGS[dcd_rt] >> dcd_shamt;
                    break;
                // Shift word right arithmetic
                case SUBOP_SRA:
                    NEXT_STATE.REGS[dcd_rd] = (int32_t)CURRENT_STATE.REGS[dcd_rt] >> dcd_shamt;
                    break;
                // Multiply unsigned word.
                case SUBOP_MULTU:
                    
                    // unsigned -> Rs * Rt
                    // the low order 32bit is placed to special register LO
                    // high order 32bit is placed to special register HI
                    mult_temp = (uint64_t)CURRENT_STATE.REGS[dcd_rs] * (uint64_t)CURRENT_STATE.REGS[dcd_rt];
                    // mult_temp  0x1fffffffe
                    // NEXT_STATE.HI = mult_temp&0xFFFFFFFF00000000;
                    NEXT_STATE.HI = mult_temp >> 32;
                    NEXT_STATE.LO = mult_temp&0x00000000FFFFFFFF;
                    break;
                // Didive word
                case SUBOP_DIVU:
                    //error throwing if the target register(Rt) is 0
                    if (dcd_rt == 0){
                        fprintf(stderr, "can't divided by 0\n");
                        exit(-1);
                    }
                    // Operation
                    // hi = $s % $t
                    NEXT_STATE.HI=((uint32_t)CURRENT_STATE.REGS[dcd_rs] % (uint32_t)CURRENT_STATE.REGS[dcd_rt]);
                    // lo = $s / $t
			        NEXT_STATE.LO=((uint32_t)CURRENT_STATE.REGS[dcd_rs] / (uint32_t)CURRENT_STATE.REGS[dcd_rt]);
                    break;
                // Set on less than unsigned
                // ex) sltu $1, $2, $3
                // if($2 < $3){ $1 = 1; }
                // else{ $1 = 0; }
                case SUBOP_SLTU:
                    // $d = ($s < $t)
                    if (CURRENT_STATE.REGS[dcd_rs] < CURRENT_STATE.REGS[dcd_rt]){
                        NEXT_STATE.REGS[dcd_rd] = 1;
                    } else{
                        NEXT_STATE.REGS[dcd_rd] = 0;
                    }
                    break;
                // Move From HI.
                // Copy from special resigter HI to general register
                // EX) mfhi $2
                // $2 = HI
                case SUBOP_MFHI:
                    NEXT_STATE.REGS[dcd_rd] = CURRENT_STATE.HI;
                    break;
                /*------- PART I---------*/
            }
            break;

 
        case OP_ADDI:
            NEXT_STATE.REGS[dcd_rt]=(int)CURRENT_STATE.REGS[dcd_rs]+dcd_se_imm;
            break;

        case OP_ADDIU:
            NEXT_STATE.REGS[dcd_rt] = CURRENT_STATE.REGS[dcd_rs] + dcd_se_imm;
            break;
        /*---------PART I---------*/
        case OP_ORI:
            NEXT_STATE.REGS[dcd_rt]=CURRENT_STATE.REGS[dcd_rs]|(dcd_imm&0x0000FFFF);
            break;
        /*---------PART II---------*/
        // 26-bit target address is shifted left 2 bits and
        // combined with the high-order bits of the address of the delay slot
        // unconditionally jumps to this calculated address with a delay of one instruction
        // pc += i << 2
        case OP_J:
            NEXT_STATE.PC = CURRENT_STATE.PC + (dcd_target << 2);
            // NEXT_STATE.PC = (dcd_target << 2) | (CURRENT_STATE.PC & 0xF0000000);
            break;
        // branch on equal
        // if($s == $t) -> pc += i << 2
        // else -> regular PC update(PC+4)
        case OP_BEQ:
            if (CURRENT_STATE.REGS[dcd_rs] == CURRENT_STATE.REGS[dcd_rt]){
                NEXT_STATE.PC = CURRENT_STATE.PC + (dcd_se_imm << 2);
            }
            else{
                NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            }
            break;
        // branch on not equal
        // if($s != $t) -> pc += i << 2
        case OP_BNE:
            if(CURRENT_STATE.REGS[dcd_rs] != CURRENT_STATE.REGS[dcd_rt])
                NEXT_STATE.PC = CURRENT_STATE.PC + (dcd_se_imm << 2);
            break;
        // branch on greater than zero
        // if($s > 0) -> pc += i << 2
        case OP_BGTZ:
            if(CURRENT_STATE.REGS[dcd_rs] > 0)
                NEXT_STATE.PC = CURRENT_STATE.PC + (dcd_se_imm << 2);
            break;
        // load upper immediate
        // the 16-bit 'immediate' is shifted left 16 bits
        // and concatenated to 16 bits of 'zeros'
        // the result is placed into 'Rt'
        // ex) lui $r0, 0x1234 -> r0 = ((0x1234) << 16)
        case OP_LUI:
            NEXT_STATE.REGS[dcd_rt] = (dcd_imm << 16);
            break;
        // load byte
        // $t = SE (MEM [$s + 1]:1)
        // offset is sign-extened 16 bits
        // (int32_t) makes the 32bit but signed
        case OP_LB:
            NEXT_STATE.REGS[dcd_rt] = sign_extend_b2w((mem_read_32(CURRENT_STATE.REGS[dcd_rs] + (dcd_se_imm)) & 0xFF));
            break;
        // load byte unsigned
        // 0xFF makes the value a byte
        // (uint32_t) makes the 32 bits but unsigned -> the remaining bits are filled with all 0s
        case OP_LBU:
            NEXT_STATE.REGS[dcd_rt] = (uint32_t)(mem_read_32(CURRENT_STATE.REGS[dcd_rs] + dcd_se_imm) & 0xFF);
            break;
        // load word
        
        // $t = MEM [$s + i]:4
        // since it's dealing with the word, we don't have to manipulate it
        case OP_LW:
            NEXT_STATE.REGS[dcd_rt] = mem_read_32(CURRENT_STATE.REGS[dcd_rs] + dcd_se_imm);
            break;
        // store byte
        // 16-bit 'offset' is sign-extended and
        // added to the contents of general register 'base'
        // to form a virtual address.
        // The least-significant byte of register 'Rt' is stored at the effective address
        case OP_SB:{
            uint32_t virtualAddress = dcd_se_imm + CURRENT_STATE.REGS[dcd_rs];
            // & 0xFF leaves the least significant byte(8bits)
            // since it is storing a byte
            mem_write_32((virtualAddress), ((CURRENT_STATE.REGS[dcd_rt]) & 0xFF));
            break;
        }
        // store word 
        // 16- 'offset' is sign-extended and
        // added to the contents of general register 'base'
        // to form a virtual address
        case OP_SW:
            virtualAddress = dcd_se_imm + CURRENT_STATE.REGS[dcd_rs];
            mem_write_32(virtualAddress, (int)CURRENT_STATE.REGS[dcd_rt]);
            // if either of the two least-significant bits of the effective address are non-zero,
            // and address error exception occurs        
            break;
        /*---------PART II END---------*/

        /*-------BONUS--------*/
        // set on less than immediate
        // $t = ($s < sign extended immediate), either 1 or 0
        case OP_SLTI:
            if((int32_t)CURRENT_STATE.REGS[dcd_rs] < (int32_t)dcd_se_imm)
                NEXT_STATE.REGS[dcd_rt] = 1;
            else 
                NEXT_STATE.REGS[dcd_rt] = 0;
            break;
        // set on less than immediate unsigned
        case OP_SLTIU:
            if(CURRENT_STATE.REGS[dcd_rs] < dcd_se_imm)
                NEXT_STATE.REGS[dcd_rt] = 1;
            else
                NEXT_STATE.REGS[dcd_rt] = 0;
            break;
        // branch on less than Or equal to zero
        // if ($s <= 0) pc += i << 2
        case OP_BLEZ:
            if(CURRENT_STATE.REGS[dcd_rs] <= 0){
                NEXT_STATE.PC += (dcd_se_imm << 2);
            }
            break;
        // branch on greater than Or equal to zero
        // if ($s >= 0) pc += i << 2
        case OP_BGEZ:
            if(CURRENT_STATE.REGS[dcd_rs] >= 0){
                NEXT_STATE.PC += (dcd_se_imm << 2);
            }
            break;
        }
        /*-------BONUS END--------*/


        
        CURRENT_STATE.REGS[0] = 0;
}

void process_instruction()
{
    fetch();
    decode();
    execute();
    /* execute one instruction here. You should use CURRENT_STATE and modify
     * values in NEXT_STATE. You can call mem_read_32() and mem_write_32() to
     * access memory. */
}
