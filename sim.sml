type register = (string * int)
type flag = (string * int)
type state = {flags:flag list, regs:register list}

datatype Operand = Reg of string | Imm of int | Mem of (string * int * int)

local
    fun fRegAcc (acc, s:string, (rid, rval)::rs) =
      if s = rid then SOME (rev acc, (rid, rval), rs)
      else fRegAcc ((rid, rval)::acc, s, rs)
      | fRegAcc (_, _, []) = NONE
in
fun findReg (s:string, rs:register list):(register list * register * register list) option = fRegAcc ([], s, rs)
end

fun printRegs ((rid, rval)::rs:register list) = (print (rid ^ ": " ^ Int.toString(rval) ^ "\n"); printRegs rs)
  | printRegs nil = ()

exception InvalidRegister
exception InvalidInstr

signature SIMULATOR = sig
    type operation
    (* Function to step through the simulation *)
    val step : (state * operation list) -> (state * operation list)
    (* Function to display the simulator state *)
    val print : (state * operation list) -> unit
    val parseOprString : string -> operation list
end

(* Operation string format:
 OPR dst src
 OPR2 dst2, src2...*)

structure ExSim :> SIMULATOR = struct
datatype operation = Add of Operand * Operand
                   | Sub of Operand * Operand
                   | Mov of Operand * Operand
(* Step the simulator, applying the top operation to the machine context*)
fun step (ctx:state, Add (Reg l, Imm i)::oprs):state * (operation list)  =
  (* Add Reg, Imm *)
  let
      val flags = #flags ctx;
      val (left, reg, right) = case findReg (l, #regs ctx) of
                                   SOME(l, g, r) => (l, g, r)
                                 | NONE => raise InvalidRegister;
      val (rid, rval) = reg;
      val regs = left@((rid, rval + i)::right);
  in
      ({flags=flags, regs=regs}, oprs)
  end
  (* Add Reg, Reg *)
  | step (ctx, Add (Reg l, Reg r)::oprs) =
    let
        val flags = #flags ctx;
        val (left, (drid, drval), right) =
            case findReg (l, #regs ctx) of
                SOME(l, g, r) => (l, g, r)
              | NONE => raise InvalidRegister;
        val (_, (_, sval), _) =
            case findReg (r, #regs ctx) of
                SOME(l, g, r) => (l, g, r)
              | NONE => raise InvalidRegister;
        val regs = left@((drid, drval + sval)::right);
    in
        ({flags=flags, regs=regs}, oprs)
    end
  (* Sub Reg, Reg*)
  | step (ctx, Sub (Reg l, Reg r)::oprs) =
    let
        val flags = #flags ctx;
        val (left, (drid, drval), right) =
            case findReg (l, #regs ctx) of
                SOME(l, g, r) => (l, g, r)
              | NONE => raise InvalidRegister;
        val (_, (_, sval), _) =
            case findReg (r, #regs ctx) of
                SOME(l, g, r) => (l, g, r)
              | NONE => raise InvalidRegister;
        val regs = left@((drid, drval - sval)::right);
    in
        ({flags=flags, regs=regs}, oprs)
    end
  (* Sub Reg, Imm *)
  | step (ctx, Sub (Reg l, Imm i)::oprs) =
    let
        val flags = #flags ctx;
        val (left, reg, right) = case findReg (l, #regs ctx) of
                                     SOME(l, g, r) => (l, g, r)
                                   | NONE => raise InvalidRegister;
        val (rid, rval) = reg;
        val regs = left@((rid, rval - i)::right);
    in
        ({flags=flags, regs=regs}, oprs)
    end
  | step _ = raise InvalidInstr
fun print (ctx, oprs) = ()(*printState ctx; printOps oprs*)
fun parseOprString s = [];
end (* basic loop: val (s, oprs) = ExSim.step (s, oprs) *)
