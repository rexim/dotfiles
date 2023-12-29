;;; fasm-mode.el --- Fasm major mode

;; Author: Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/Fanael/fasm-mode
;; Version: 0.1.8

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2013, Fanael Linithien
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;   * Neither the name of the copyright holder(s) nor the names of any
;;     contributors may be used to endorse or promote products derived from
;;     this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(defvar fasm-mode-syntax-table
  (let ((syntaxtable (make-syntax-table)))
    (modify-syntax-entry ?_ "_" syntaxtable)
    (modify-syntax-entry ?. "_" syntaxtable)
    (modify-syntax-entry ?$ "_" syntaxtable)
    (modify-syntax-entry ?@ "_" syntaxtable)
    (modify-syntax-entry ?~ "_" syntaxtable)
    (modify-syntax-entry ?? "_" syntaxtable)
    (modify-syntax-entry ?! "_" syntaxtable)
    (modify-syntax-entry ?= "." syntaxtable)
    (modify-syntax-entry ?+ "." syntaxtable)
    (modify-syntax-entry ?- "." syntaxtable)
    (modify-syntax-entry ?* "." syntaxtable)
    (modify-syntax-entry ?/ "." syntaxtable)
    (modify-syntax-entry ?\\ "." syntaxtable)
    (modify-syntax-entry ?\; "<" syntaxtable)
    (modify-syntax-entry ?\n ">" syntaxtable)
    (modify-syntax-entry ?\" "\"" syntaxtable)
    (modify-syntax-entry ?\' "\"" syntaxtable)
    syntaxtable)
  "Syntax table for FASM mode.")

(defvar fasm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") (lambda () (interactive) (insert ?\n)))
    map)
  "Local keymap for FASM mode.")

(defvar fasm-basic-offset 2
  "Offset for FASM mode indentation.")

(eval-when-compile
  (defun fasm--regexp-from-keywords (&rest keywords)
    (regexp-opt keywords 'symbols)))

(defconst fasm-font-lock-keywords
  `(;; Numbers
    ("\\_<[01]+b\\_>" . 'font-lock-constant-face)
    ("\\_<[0-9][0-9a-fA-F]*h\\_>" . 'font-lock-constant-face)
    ("\\_<\\(?:0x\\|\\$\\)[0-9a-fA-F]+\\_>" . 'font-lock-constant-face)
    ("\\_<[0-9]+\\(?:\\.[0-9]*\\)?\\(?:e[+-]?[0-9]+\\)?\\_>"
     . 'font-lock-constant-face)
    ;; Types
    (,(eval-when-compile
       (fasm--regexp-from-keywords
        "xx"
        "xxxx"
        "preserve"
        "byte" "word" "dword" "fword" "pword" "qword" "tbyte" "tword"
        "dqword" "xword" "qqword" "yword" "db" "rb" "dw" "du" "rw" "dd" "rd"
        "df" "dp" "rf" "rp" "dq" "rq" "dt" "rt")) . 'font-lock-type-face)
    ;; Directives and operators
    (,(eval-when-compile
       (fasm--regexp-from-keywords
        "labeling"
        "mod" "rva" "plt" "align" "as" "at" "defined" "dup" "eq" "eqtype"
        "from" "ptr" "relativeto" "used" "binary" "export" "fixups" "import"
        "native" "static" "console" "dynamic" "efiboot" "linkinfo" "readable"
        "resource" "writable" "shareable" "writeable" "efiruntime"
        "executable" "linkremove" "discardable" "interpreter" "notpageable"

        "if"
        "end"
        "tail_call"
        "finish" ;; as end

        "literal"
        "address"
        "branch"
        "false?branch"

        "err" "org" "data" "else" "heap" "load" "align" "break"
        "entry" "extrn" "label" "stack" "store" "times" "while" "assert"
        "format" "public" "repeat" "display" "section" "segment" "virtual"
        "file")) . 'font-lock-keyword-face)
    ;; Preprocessor directives
    (,(eval-when-compile
       (fasm--regexp-from-keywords
        "define" "include" "irp" "irps" "macro" "match" "purge" "rept"
        "restore" "restruc" "struc" "common" "forward" "local" "reverse"
        "equ" "fix")) . 'font-lock-preprocessor-face)
    ;; Registers
    (,(eval-when-compile
       (fasm--regexp-from-keywords
        "al" "bl" "cl" "dl" "spl" "bpl" "sil" "dil" "r8b" "r9b" "r10b" "r11b"
        "r12b" "r13b" "r14b" "r15b" "ah" "bh" "ch" "dh" "ax" "bx" "cx" "dx"
        "sp" "bp" "si" "di" "r8w" "r9w" "r10w" "r11w" "r12w" "r13w" "r14w"
        "r15w" "eax" "ebx" "ecx" "edx" "esp" "ebp" "esi" "edi" "r8d" "r9d"
        "r10d" "r11d" "r12d" "r13d" "r14d" "r15d"

        "rax" "rbx" "rcx" "rdx" "rsp" "rbp" "rsi" "rdi"
        "r8" "r9" "r10" "r11" "r12" "r13" "r14" "r15"

        ;; "xax" "xbx" "xcx" "xdx" "xsp" "xbp" "xsi" "xdi"
        ;; "x8" "x9" "x10" "x11" "x12" "x13" "x14" "x15"

        "rip" "es" "cs" "ss" "ds" "fs" "gs" "cr0" "cr2" "cr3" "cr4" "dr0"
        "dr1" "dr2" "dr3" "st0" "st1" "st2" "st3" "st4" "st5" "st6" "st7"
        "mm0" "mm1" "mm2" "mm3" "mm4" "mm5" "mm6" "mm7" "xmm0" "xmm1" "xmm2"
        "xmm3" "xmm4" "xmm5" "xmm6" "xmm7" "xmm8" "xmm9" "xmm10" "xmm11"
        "xmm12" "xmm13" "xmm14" "xmm15" "ymm0" "ymm1" "ymm2" "ymm3" "ymm4"
        "ymm5" "ymm6" "ymm7" "ymm8" "ymm9" "ymm10" "ymm11" "ymm12" "ymm13"
        "ymm14" "ymm15")) . 'font-lock-variable-name-face)
    ;; Instructions
    (,(eval-when-compile
       (fasm--regexp-from-keywords
        "bt" "in" "ja" "jb" "jc" "je" "jg" "jl" "jo" "jp" "js" "jz" "or"
        "aaa" "aad" "aam" "aas" "adc" "add" "and" "bsf" "bsr" "btc" "btr"
        "bts" "cbw" "cdq" "clc" "cld" "cli" "cmc" "cmp" "cqo" "cwd" "daa"
        "das" "dec" "div" "fld" "fst" "hlt" "inc" "ins" "int" "jae" "jbe"
        "jge" "jle" "jmp" "jna" "jnb" "jnc" "jne" "jng" "jnl" "jno" "jnp"
        "jns" "jnz" "jpe" "jpo" "lar" "lds" "lea" "les" "lfs" "lgs" "lsl"
        "lss" "ltr" "mov" "mul" "neg" "nop" "not" "out" "pop" "por" "rcl"
        "rcr" "rep" "ret" "rol" "ror" "rsm" "sal" "sar" "sbb" "shl" "shr"
        "stc" "std" "sti" "str" "sub" "ud2" "xor" "adcx" "adox" "andn" "arpl"
        "blci" "blcs" "blsi" "blsr" "bzhi" "call" "cdqe" "clac" "clgi" "clts"
        "cmps" "cwde" "dppd" "dpps" "emms" "fabs" "fadd" "fbld" "fchs" "fcom"
        "fcos" "fdiv" "feni" "fild" "fist" "fld1" "fldz" "fmul" "fnop" "fsin"
        "fstp" "fsub" "ftst" "fxam" "fxch" "idiv" "imul" "insb" "insd" "insw"
        "int1" "int3" "into" "invd" "iret" "jcxz" "jnae" "jnbe" "jnge" "jnle"
        "lahf" "lgdt" "lidt" "lldt" "lmsw" "lock" "lods" "loop" "movd" "movq"
        "movs" "mulx" "orpd" "orps" "outs" "pand" "pdep" "pext" "popa" "popd"
        "popf" "popq" "popw" "push" "pxor" "repe" "repz" "retd" "retf" "retn"
        "retq" "retw" "rorx" "sahf" "salc" "sarx" "scas" "seta" "setb" "setc"
        "sete" "setg" "setl" "seto" "setp" "sets" "setz" "sgdt" "shld" "shlx"
        "shrd" "shrx" "sidt" "sldt" "smsw" "stac" "stgi" "stos" "test" "verr"
        "verw" "vpor" "wait" "xadd" "xchg" "xend" "xlat" "addpd" "addps"
        "addsd" "addss" "andpd" "andps" "bextr" "blcic" "blsic" "bound"
        "bswap" "cmova" "cmovb" "cmovc" "cmove" "cmovg" "cmovl" "cmovo"
        "cmovp" "cmovs" "cmovz" "cmppd" "cmpps" "cmpsb" "cmpsd" "cmpsq"
        "cmpss" "cmpsw" "cpuid" "crc32" "divpd" "divps" "divsd" "divss"
        "enter" "extrq" "f2xm1" "faddp" "fbstp" "fclex" "fcomi" "fcomp"
        "fdisi" "fdivp" "fdivr" "femms" "ffree" "fiadd" "ficom" "fidiv"
        "fimul" "finit" "fistp" "fisub" "fldcw" "fldpi" "fmulp" "fneni"
        "fprem" "fptan" "fsave" "fsqrt" "fstcw" "fstsw" "fsubp" "fsubr"
        "fucom" "fwait" "fyl2x" "icebp" "iretd" "iretq" "iretw" "jecxz"
        "jrcxz" "lddqu" "leave" "lodsb" "lodsd" "lodsq" "lodsw" "loopd"
        "loope" "loopq" "loopw" "loopz" "lzcnt" "maxpd" "maxps" "maxsd"
        "maxss" "minpd" "minps" "minsd" "minss" "movbe" "movsb" "movsd"
        "movsq" "movss" "movsw" "movsx" "movzx" "mulpd" "mulps" "mulsd"
        "mulss" "mwait" "outsb" "outsd" "outsw" "pabsb" "pabsd" "pabsw"
        "paddb" "paddd" "paddq" "paddw" "pandn" "pause" "pavgb" "pavgw"
        "pf2id" "pf2iw" "pfacc" "pfadd" "pfmax" "pfmin" "pfmul" "pfrcp"
        "pfsub" "pi2fd" "pi2fw" "popad" "popaw" "popfd" "popfq" "popfw"
        "pslld" "psllq" "psllw" "psrad" "psraw" "psrld" "psrlq" "psrlw"
        "psubb" "psubd" "psubq" "psubw" "ptest" "pusha" "pushd" "pushf"
        "pushq" "pushw" "rcpps" "rcpss" "rdmsr" "rdpmc" "rdtsc" "repne"
        "repnz" "retfd" "retfq" "retfw" "retnd" "retnq" "retnw" "scasb"
        "scasd" "scasq" "scasw" "setae" "setbe" "setge" "setle" "setna"
        "setnb" "setnc" "setne" "setng" "setnl" "setno" "setnp" "setns"
        "setnz" "setpe" "setpo" "stosb" "stosd" "stosq" "stosw" "subpd"
        "subps" "subsd" "subss" "tzcnt" "tzmsk" "vdppd" "vdpps" "vmovd"
        "vmovq" "vmrun" "vmxon" "vorpd" "vorps" "vpand" "vpxor" "wrmsr"
        "xlatb" "xorpd" "xorps" "xsave" "xtest" "aesdec" "aesenc" "aesimc"
        "andnpd" "andnps" "blcmsk" "blsmsk" "cmovae" "cmovbe" "cmovge"
        "cmovle" "cmovna" "cmovnb" "cmovnc" "cmovne" "cmovng" "cmovnl"
        "cmovno" "cmovnp" "cmovns" "cmovnz" "cmovpe" "cmovpo" "comisd"
        "comiss" "fcmovb" "fcmove" "fcmovu" "fcomip" "fcompp" "fdivrp"
        "ffreep" "ficomp" "fidivr" "fisttp" "fisubr" "fldenv" "fldl2e"
        "fldl2t" "fldlg2" "fldln2" "fnclex" "fndisi" "fninit" "fnsave"
        "fnstcw" "fnstsw" "fpatan" "fprem1" "frstor" "frstpm" "fsaved"
        "fsavew" "fscale" "fsetpm" "fstenv" "fsubrp" "fucomi" "fucomp"
        "fxsave" "getsec" "haddpd" "haddps" "hsubpd" "hsubps" "invept"
        "invlpg" "lfence" "llwpcb" "looped" "loopeq" "loopew" "loopne"
        "loopnz" "loopzd" "loopzq" "loopzw" "lwpins" "lwpval" "mfence"
        "movapd" "movaps" "movdqa" "movdqu" "movhpd" "movhps" "movlpd"
        "movlps" "movnti" "movntq" "movsxd" "movupd" "movups" "paddsb"
        "paddsw" "pextrb" "pextrd" "pextrq" "pextrw" "pfnacc" "pfsubr"
        "phaddd" "phaddw" "phsubd" "phsubw" "pinsrb" "pinsrd" "pinsrq"
        "pinsrw" "pmaxsb" "pmaxsd" "pmaxsw" "pmaxub" "pmaxud" "pmaxuw"
        "pminsb" "pminsd" "pminsw" "pminub" "pminud" "pminuw" "pmuldq"
        "pmulhw" "pmulld" "pmullw" "popcnt" "psadbw" "pshufb" "pshufd"
        "pshufw" "psignb" "psignd" "psignw" "pslldq" "psrldq" "psubsb"
        "psubsw" "pswapd" "pushad" "pushaw" "pushfd" "pushfq" "pushfw"
        "rdmsrq" "rdrand" "rdseed" "rdtscp" "setalc" "setnae" "setnbe"
        "setnge" "setnle" "sfence" "shufpd" "shufps" "skinit" "slwpcb"
        "sqrtpd" "sqrtps" "sqrtsd" "sqrtss" "swapgs" "sysret" "t1mskc"
        "vaddpd" "vaddps" "vaddsd" "vaddss" "vandpd" "vandps" "vcmppd"
        "vcmpps" "vcmpsd" "vcmpss" "vdivpd" "vdivps" "vdivsd" "vdivss"
        "vlddqu" "vmaxpd" "vmaxps" "vmaxsd" "vmaxss" "vmcall" "vminpd"
        "vminps" "vminsd" "vminss" "vmload" "vmovsd" "vmovss" "vmread"
        "vmsave" "vmulpd" "vmulps" "vmulsd" "vmulss" "vmxoff" "vpabsb"
        "vpabsd" "vpabsw" "vpaddb" "vpaddd" "vpaddq" "vpaddw" "vpandn"
        "vpavgb" "vpavgw" "vpcmov" "vpcomb" "vpcomd" "vpcomq" "vpcomw"
        "vpermd" "vpermq" "vpperm" "vprotb" "vprotd" "vprotq" "vprotw"
        "vpshab" "vpshad" "vpshaq" "vpshaw" "vpshlb" "vpshld" "vpshlq"
        "vpshlw" "vpslld" "vpsllq" "vpsllw" "vpsrad" "vpsraw" "vpsrld"
        "vpsrlq" "vpsrlw" "vpsubb" "vpsubd" "vpsubq" "vpsubw" "vptest"
        "vrcpps" "vrcpss" "vsubpd" "vsubps" "vsubsd" "vsubss" "vxorpd"
        "vxorps" "wbinvd" "wrmsrq" "xabort" "xbegin" "xgetbv" "xrstor"
        "xsetbv" "blcfill" "blendpd" "blendps" "blsfill" "clflush" "cmovnae"
        "cmovnbe" "cmovnge" "cmovnle" "cmpeqpd" "cmpeqps" "cmpeqsd" "cmpeqss"
        "cmplepd" "cmpleps" "cmplesd" "cmpless" "cmpltpd" "cmpltps" "cmpltsd"
        "cmpltss" "cmpxchg" "fcmovbe" "fcmovnb" "fcmovne" "fcmovnu" "fdecstp"
        "fincstp" "fldenvd" "fldenvw" "fnsaved" "fnsavew" "fnstenv" "frndint"
        "frstord" "frstorw" "fsincos" "fstenvd" "fstenvw" "fucomip" "fucompp"
        "fxrstor" "fxtract" "fyl2xp1" "insertq" "invlpga" "invpcid" "invvpid"
        "ldmxcsr" "loopned" "loopneq" "loopnew" "loopnzd" "loopnzq" "loopnzw"
        "monitor" "movddup" "movdq2q" "movhlps" "movlhps" "movntdq" "movntpd"
        "movntps" "movntsd" "movntss" "movq2dq" "mpsadbw" "paddusb" "paddusw"
        "palignr" "pavgusb" "pblendw" "pcmpeqb" "pcmpeqd" "pcmpeqq" "pcmpeqw"
        "pcmpgtb" "pcmpgtd" "pcmpgtq" "pcmpgtw" "pfcmpeq" "pfcmpge" "pfcmpgt"
        "pfpnacc" "pfrsqrt" "phaddsw" "phsubsw" "pmaddwd" "pmulhrw" "pmulhuw"
        "pmuludq" "pshufhw" "pshuflw" "psubusb" "psubusw" "roundpd" "roundps"
        "roundsd" "roundss" "rsqrtps" "rsqrtss" "stmxcsr" "syscall" "sysexit"
        "sysretq" "ucomisd" "ucomiss" "vaesdec" "vaesenc" "vaesimc" "vandnpd"
        "vandnps" "vcomisd" "vcomiss" "vfrczpd" "vfrczps" "vfrczsd" "vfrczss"
        "vhaddpd" "vhaddps" "vhsubpd" "vhsubps" "vmclear" "vmmcall" "vmovapd"
        "vmovaps" "vmovdqa" "vmovdqu" "vmovhpd" "vmovhps" "vmovlpd" "vmovlps"
        "vmovupd" "vmovups" "vmptrld" "vmptrst" "vmwrite" "vpaddsb" "vpaddsw"
        "vpcomub" "vpcomud" "vpcomuq" "vpcomuw" "vpermpd" "vpermps" "vpextrb"
        "vpextrd" "vpextrq" "vpextrw" "vphaddd" "vphaddw" "vphsubd" "vphsubw"
        "vpinsrb" "vpinsrd" "vpinsrq" "vpinsrw" "vpmaxsb" "vpmaxsd" "vpmaxsw"
        "vpmaxub" "vpmaxud" "vpmaxuw" "vpminsb" "vpminsd" "vpminsw" "vpminub"
        "vpminud" "vpminuw" "vpmuldq" "vpmulhw" "vpmulld" "vpmullw" "vpsadbw"
        "vpshufb" "vpshufd" "vpsignb" "vpsignd" "vpsignw" "vpslldq" "vpsllvd"
        "vpsllvq" "vpsravd" "vpsrldq" "vpsrlvd" "vpsrlvq" "vpsubsb" "vpsubsw"
        "vshufpd" "vshufps" "vsqrtpd" "vsqrtps" "vsqrtsd" "vsqrtss" "vtestpd"
        "vtestps" "xsave64" "addsubpd" "addsubps" "blendvpd" "blendvps"
        "cmpneqpd" "cmpneqps" "cmpneqsd" "cmpneqss" "cmpnlepd" "cmpnleps"
        "cmpnlesd" "cmpnless" "cmpnltpd" "cmpnltps" "cmpnltsd" "cmpnltss"
        "cmpordpd" "cmpordps" "cmpordsd" "cmpordss" "cvtdq2pd" "cvtdq2ps"
        "cvtpd2dq" "cvtpd2pi" "cvtpd2ps" "cvtpi2pd" "cvtpi2ps" "cvtps2dq"
        "cvtps2pd" "cvtps2pi" "cvtsd2si" "cvtsd2ss" "cvtsi2sd" "cvtsi2ss"
        "cvtss2sd" "cvtss2si" "fcmovnbe" "fnstenvd" "fnstenvw" "fxsave64"
        "insertps" "maskmovq" "movmskpd" "movmskps" "movntdqa" "movshdup"
        "movsldup" "packssdw" "packsswb" "packusdw" "packuswb" "pblendvb"
        "pfrcpit1" "pfrcpit2" "pfrsqit1" "pmovmskb" "pmovsxbd" "pmovsxbq"
        "pmovsxbw" "pmovsxdq" "pmovsxwd" "pmovsxwq" "pmovzxbd" "pmovzxbq"
        "pmovzxbw" "pmovzxdq" "pmovzxwd" "pmovzxwq" "pmulhrsw" "prefetch"
        "rdfsbase" "rdgsbase" "sysenter" "sysexitq" "unpckhpd" "unpckhps"
        "unpcklpd" "unpcklps" "vblendpd" "vblendps" "vcmpeqpd" "vcmpeqps"
        "vcmpeqsd" "vcmpeqss" "vcmpgepd" "vcmpgeps" "vcmpgesd" "vcmpgess"
        "vcmpgtpd" "vcmpgtps" "vcmpgtsd" "vcmpgtss" "vcmplepd" "vcmpleps"
        "vcmplesd" "vcmpless" "vcmpltpd" "vcmpltps" "vcmpltsd" "vcmpltss"
        "vfmaddpd" "vfmaddps" "vfmaddsd" "vfmaddss" "vfmsubpd" "vfmsubps"
        "vfmsubsd" "vfmsubss" "vldmxcsr" "vmlaunch" "vmovddup" "vmovhlps"
        "vmovlhps" "vmovntdq" "vmovntpd" "vmovntps" "vmpsadbw" "vmresume"
        "vpaddusb" "vpaddusw" "vpalignr" "vpblendd" "vpblendw" "vpcmpeqb"
        "vpcmpeqd" "vpcmpeqq" "vpcmpeqw" "vpcmpgtb" "vpcmpgtd" "vpcmpgtq"
        "vpcmpgtw" "vpcomeqb" "vpcomeqd" "vpcomeqq" "vpcomeqw" "vpcomgeb"
        "vpcomged" "vpcomgeq" "vpcomgew" "vpcomgtb" "vpcomgtd" "vpcomgtq"
        "vpcomgtw" "vpcomleb" "vpcomled" "vpcomleq" "vpcomlew" "vpcomltb"
        "vpcomltd" "vpcomltq" "vpcomltw" "vphaddbd" "vphaddbq" "vphaddbw"
        "vphadddq" "vphaddsw" "vphaddwd" "vphaddwq" "vphsubbw" "vphsubdq"
        "vphsubsw" "vphsubwd" "vpmacsdd" "vpmacswd" "vpmacsww" "vpmaddwd"
        "vpmulhuw" "vpmuludq" "vpshufhw" "vpshuflw" "vpsubusb" "vpsubusw"
        "vroundpd" "vroundps" "vroundsd" "vroundss" "vrsqrtps" "vrsqrtss"
        "vstmxcsr" "vucomisd" "vucomiss" "vzeroall" "wrfsbase" "wrgsbase"
        "xacquire" "xrelease" "xrstor64" "xsaveopt" "cmpxchg8b" "cvttpd2dq"
        "cvttpd2pi" "cvttps2dq" "cvttps2pi" "cvttsd2si" "cvttss2si"
        "extractps" "fxrstor64" "pclmulqdq" "pcmpestri" "pcmpestrm"
        "pcmpistri" "pcmpistrm" "pmaddubsw" "prefetchw" "punpckhbw"
        "punpckhdq" "punpckhwd" "punpcklbw" "punpckldq" "punpcklwd"
        "vaddsubpd" "vaddsubps" "vblendvpd" "vblendvps" "vcmpneqpd"
        "vcmpneqps" "vcmpneqsd" "vcmpneqss" "vcmpngepd" "vcmpngeps"
        "vcmpngesd" "vcmpngess" "vcmpngtpd" "vcmpngtps" "vcmpngtsd"
        "vcmpngtss" "vcmpnlepd" "vcmpnleps" "vcmpnlesd" "vcmpnless"
        "vcmpnltpd" "vcmpnltps" "vcmpnltsd" "vcmpnltss" "vcmpordpd"
        "vcmpordps" "vcmpordsd" "vcmpordss" "vcvtdq2pd" "vcvtdq2ps"
        "vcvtpd2dq" "vcvtpd2ps" "vcvtph2ps" "vcvtps2dq" "vcvtps2pd"
        "vcvtps2ph" "vcvtsd2si" "vcvtsd2ss" "vcvtsi2sd" "vcvtsi2ss"
        "vcvtss2sd" "vcvtss2si" "vfnmaddpd" "vfnmaddps" "vfnmaddsd"
        "vfnmaddss" "vfnmsubpd" "vfnmsubps" "vfnmsubsd" "vfnmsubss"
        "vinsertps" "vmovmskpd" "vmovmskps" "vmovntdqa" "vmovshdup"
        "vmovsldup" "vpackssdw" "vpacksswb" "vpackusdw" "vpackuswb"
        "vpblendvb" "vpcomequb" "vpcomequd" "vpcomequq" "vpcomequw"
        "vpcomgeub" "vpcomgeud" "vpcomgeuq" "vpcomgeuw" "vpcomgtub"
        "vpcomgtud" "vpcomgtuq" "vpcomgtuw" "vpcomleub" "vpcomleud"
        "vpcomleuq" "vpcomleuw" "vpcomltub" "vpcomltud" "vpcomltuq"
        "vpcomltuw" "vpcomneqb" "vpcomneqd" "vpcomneqq" "vpcomneqw"
        "vpermilpd" "vpermilps" "vphaddubd" "vphaddubq" "vphaddubw"
        "vphaddudq" "vphadduwd" "vphadduwq" "vpmacsdqh" "vpmacsdql"
        "vpmacssdd" "vpmacsswd" "vpmacssww" "vpmadcswd" "vpmovmskb"
        "vpmovsxbd" "vpmovsxbq" "vpmovsxbw" "vpmovsxdq" "vpmovsxwd"
        "vpmovsxwq" "vpmovzxbd" "vpmovzxbq" "vpmovzxbw" "vpmovzxdq"
        "vpmovzxwd" "vpmovzxwq" "vpmulhrsw" "vunpckhpd" "vunpckhps"
        "vunpcklpd" "vunpcklps" "aesdeclast" "aesenclast" "cmpunordpd"
        "cmpunordps" "cmpunordsd" "cmpunordss" "cmpxchg16b" "loadall286"
        "loadall386" "maskmovdqu" "phminposuw" "prefetcht0" "prefetcht1"
        "prefetcht2" "punpckhqdq" "punpcklqdq" "vcmptruepd" "vcmptrueps"
        "vcmptruesd" "vcmptruess" "vcvttpd2dq" "vcvttps2dq" "vcvttsd2si"
        "vcvttss2si" "vextractps" "vgatherdpd" "vgatherdps" "vgatherqpd"
        "vgatherqps" "vmaskmovpd" "vmaskmovps" "vpclmulqdq" "vpcmpestri"
        "vpcmpestrm" "vpcmpistri" "vpcmpistrm" "vpcomnequb" "vpcomnequd"
        "vpcomnequq" "vpcomnequw" "vpcomtrueb" "vpcomtrued" "vpcomtrueq"
        "vpcomtruew" "vperm2f128" "vperm2i128" "vpermil2pd" "vpermil2ps"
        "vpgatherdd" "vpgatherdq" "vpgatherqd" "vpgatherqq" "vpmacssdqh"
        "vpmacssdql" "vpmadcsswd" "vpmaddubsw" "vpmaskmovd" "vpmaskmovq"
        "vpunpckhbw" "vpunpckhdq" "vpunpckhwd" "vpunpcklbw" "vpunpckldq"
        "vpunpcklwd" "vzeroupper" "xsaveopt64" "pclmulhqhdq" "pclmullqhdq"
        "prefetchnta" "vaesdeclast" "vaesenclast" "vcmpeq_ospd" "vcmpeq_osps"
        "vcmpeq_ossd" "vcmpeq_osss" "vcmpeq_uqpd" "vcmpeq_uqps" "vcmpeq_uqsd"
        "vcmpeq_uqss" "vcmpeq_uspd" "vcmpeq_usps" "vcmpeq_ussd" "vcmpeq_usss"
        "vcmpfalsepd" "vcmpfalseps" "vcmpfalsesd" "vcmpfalsess" "vcmpge_oqpd"
        "vcmpge_oqps" "vcmpge_oqsd" "vcmpge_oqss" "vcmpgt_oqpd" "vcmpgt_oqps"
        "vcmpgt_oqsd" "vcmpgt_oqss" "vcmple_oqpd" "vcmple_oqps" "vcmple_oqsd"
        "vcmple_oqss" "vcmplt_oqpd" "vcmplt_oqps" "vcmplt_oqsd" "vcmplt_oqss"
        "vcmpord_spd" "vcmpord_sps" "vcmpord_ssd" "vcmpord_sss" "vcmpunordpd"
        "vcmpunordps" "vcmpunordsd" "vcmpunordss" "vfmadd132pd" "vfmadd132ps"
        "vfmadd132sd" "vfmadd132ss" "vfmadd213pd" "vfmadd213ps" "vfmadd213sd"
        "vfmadd213ss" "vfmadd231pd" "vfmadd231ps" "vfmadd231sd" "vfmadd231ss"
        "vfmaddsubpd" "vfmaddsubps" "vfmsub132pd" "vfmsub132ps" "vfmsub132sd"
        "vfmsub132ss" "vfmsub213pd" "vfmsub213ps" "vfmsub213sd" "vfmsub213ss"
        "vfmsub231pd" "vfmsub231ps" "vfmsub231sd" "vfmsub231ss" "vfmsubaddpd"
        "vfmsubaddps" "vinsertf128" "vinserti128" "vmaskmovdqu" "vpcomfalseb"
        "vpcomfalsed" "vpcomfalseq" "vpcomfalsew" "vpcomtrueub" "vpcomtrueud"
        "vpcomtrueuq" "vpcomtrueuw" "vphminposuw" "vpunpckhqdq" "vpunpcklqdq"
        "pclmulhqhqdq" "pclmulhqlqdq" "pclmullqhqdq" "pclmullqlqdq"
        "vbroadcastsd" "vbroadcastss" "vcmpneq_oqpd" "vcmpneq_oqps"
        "vcmpneq_oqsd" "vcmpneq_oqss" "vcmpneq_ospd" "vcmpneq_osps"
        "vcmpneq_ossd" "vcmpneq_osss" "vcmpneq_uspd" "vcmpneq_usps"
        "vcmpneq_ussd" "vcmpneq_usss" "vcmpnge_uqpd" "vcmpnge_uqps"
        "vcmpnge_uqsd" "vcmpnge_uqss" "vcmpngt_uqpd" "vcmpngt_uqps"
        "vcmpngt_uqsd" "vcmpngt_uqss" "vcmpnle_uqpd" "vcmpnle_uqps"
        "vcmpnle_uqsd" "vcmpnle_uqss" "vcmpnlt_uqpd" "vcmpnlt_uqps"
        "vcmpnlt_uqsd" "vcmpnlt_uqss" "vextractf128" "vextracti128"
        "vfnmadd132pd" "vfnmadd132ps" "vfnmadd132sd" "vfnmadd132ss"
        "vfnmadd213pd" "vfnmadd213ps" "vfnmadd213sd" "vfnmadd213ss"
        "vfnmadd231pd" "vfnmadd231ps" "vfnmadd231sd" "vfnmadd231ss"
        "vfnmsub132pd" "vfnmsub132ps" "vfnmsub132sd" "vfnmsub132ss"
        "vfnmsub213pd" "vfnmsub213ps" "vfnmsub213sd" "vfnmsub213ss"
        "vfnmsub231pd" "vfnmsub231ps" "vfnmsub231sd" "vfnmsub231ss"
        "vpbroadcastb" "vpbroadcastd" "vpbroadcastq" "vpbroadcastw"
        "vpclmulhqhdq" "vpclmullqhdq" "vpcomfalseub" "vpcomfalseud"
        "vpcomfalseuq" "vpcomfalseuw" "vpermilmo2pd" "vpermilmo2ps"
        "vpermilmz2pd" "vpermilmz2ps" "vpermiltd2pd" "vpermiltd2ps"
        "vcmptrue_uspd" "vcmptrue_usps" "vcmptrue_ussd" "vcmptrue_usss"
        "vcmpunord_spd" "vcmpunord_sps" "vcmpunord_ssd" "vcmpunord_sss"
        "vpclmulhqlqdq" "vpclmullqlqdq" "vbroadcastf128" "vbroadcasti128"
        "vcmpfalse_ospd" "vcmpfalse_osps" "vcmpfalse_ossd" "vcmpfalse_osss"
        "vfmaddsub132pd" "vfmaddsub132ps" "vfmaddsub213pd" "vfmaddsub213ps"
        "vfmaddsub231pd" "vfmaddsub231ps" "vfmsubadd132pd" "vfmsubadd132ps"
        "vfmsubadd213pd" "vfmsubadd213ps" "vfmsubadd231pd" "vfmsubadd231ps"
        "aeskeygenassist" "vaeskeygenassist")) . 'font-lock-builtin-face)
    ;; Labels
    ("^[ \t]*\\([a-z$A-Z0-9.?!@]\\(?:\\sw\\|\\s_\\)*\\):"
     . (1 'font-lock-function-name-face))
    ;; Macro names
    ("\\(?:macro\\|struc\\)[ \t]+\\([a-zA-Z0-9.?!@]\\(?:\\sw\\|\\s_\\)*\\)"
     . (1 'font-lock-function-name-face)))
  "Syntax highlighting for FASM mode.")




;; (defun fasm--get-indent-level (lineoffset)
;;   (save-excursion
;;     (forward-line (1- lineoffset))
;;     (back-to-indentation)
;;     (current-column)))

(defun empty-line? ()
  (save-excursion
    (let ((column-a (progn (move-beginning-of-line nil)
                           (current-column)))
          (column-b (progn (back-to-indentation)
                           (current-column))))
      ;; 下面这个谓词看似是冗余的 但是没有关系
      (and (= column-b column-a)
           (eq (following-char) 10))
      )))

(defun above-char ()
  (interactive)
  (save-excursion
    (backward-char 1)
    (following-char)))

(defun fasm-label? ()
  (interactive)
  (save-excursion
    (and (progn
           (whitespace-cleanup)
           (not (empty-line?)))
         (progn
           ;; (whitespace-cleanup)
           (end-of-line)
           (eq 58 ;; 58 -- ":"
               (above-char))))))

;; (defun' test-fasm-get-indent ()
;;   (interactive)
;;   (message "><><><"))
(defun fasm-get-indent (n)
  (save-excursion
    (forward-line n)
    (cond ((empty-line?)
           "empty-line")
          ((fasm-label?)
           "fasm-label")
          (t
           (progn
             (back-to-indentation)
             (current-column))))))

(defun fasm-indent-line ()
  "Indent according to FASM major mode."
  (interactive)
  ;; 可以发现缩进函数根本就写不好 因为emacs不能真正明白文本的语法
  (cond
    ;; 下面是根据当前一行来缩进
    ((fasm-label?)
     (let ()
       (back-to-indentation)
       (delete-backward-char (current-column))))
    ;; 下面是根据上一行来缩进
    (t;; else
     (back-to-indentation)
     (let ((prev-indent (fasm-get-indent -1))
           (curr-indent (current-column)))
       (cond ((equal prev-indent "empty-line")
              ;; (indent-to 8)
              (indent-to 3)
              )
             ((equal prev-indent "fasm-label")
              ;; (indent-to 0)
              ;; 上面的那个命令根本不会缩进 这样看似是合理的 所以增加下面这句:
              (message "do not indent at all !"))
             ((< curr-indent prev-indent)
              (indent-to prev-indent))
             ((>= curr-indent prev-indent)
              (delete-backward-char (- curr-indent prev-indent)))
             ((>= curr-indent prev-indent)
              (delete-backward-char (- curr-indent prev-indent)))
             )))))

;; Emacs < 24 did not have prog-mode
(defalias 'fasm-parent-mode
    (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode fasm-mode fasm-parent-mode
  "Fasm"
  "Major mode for editing assembly in FASM format."
  (set (make-local-variable 'font-lock-defaults)
       (list 'fasm-font-lock-keywords nil t))
  (set (make-local-variable 'indent-line-function) 'fasm-indent-line)
  (set (make-local-variable 'comment-start) ";; "))

(provide 'fasm-mode)
;;; fasm-mode.el ends here
