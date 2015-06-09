type register = (string * int)
type flag = (string * int)
type state = {flags:flag list, regs:register list}

datatype Operand = Reg of string | Imm of int
datatype Operation = Add of Operand * Operand
                   | Sub of Operand * Operand
                   | Mov of Operand * Operand

local
    fun fRegAcc (acc, s, (rid, rval)::rs) =
      if s = rid then SOME (rev acc, (rid, rval), rs)
      else fRegAcc ((rid, rval)::acc, s, rs)
      | fRegAcc (_, _, []) = NONE
in
    fun findReg (s:string, rs:register list):(register list * register * register list) option = fRegAcc ([], s, rs)
end

exception InvalidRegister
exception InvalidInstr

structure ExSim = struct
(* Step the simulator, applying the top operation to the machine context*)
fun step (ctx:state, Add (Reg l, Imm i)::oprs:Operation list):state * Operation list =
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
  | step _ = raise InvalidInstr
end

(* basic loop:
 val (s, oprs) = ExSim.step (s, oprs)
*)
