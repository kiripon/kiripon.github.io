	.text
	.file	"tls.cpp"
	.globl	_Z8tls_funcv            // -- Begin function _Z8tls_funcv
	.p2align	2
	.type	_Z8tls_funcv,@function
_Z8tls_funcv:                           // @_Z8tls_funcv
// %bb.0:
	stp	x29, x30, [sp, #-16]!   // 16-byte Folded Spill
	mov	x29, sp
	adrp	x0, :tlsdesc:tls_value
	ldr	x1, [x0, :tlsdesc_lo12:tls_value]
	add	x0, x0, :tlsdesc_lo12:tls_value
	.tlsdesccall tls_value
	blr	x1
	mrs	x8, TPIDR_EL0
	ldr	x0, [x8, x0]
	ldp	x29, x30, [sp], #16     // 16-byte Folded Reload
	ret
.Lfunc_end0:
	.size	_Z8tls_funcv, .Lfunc_end0-_Z8tls_funcv
                                        // -- End function
	.section	.text._ZTW9tls_value,"axG",@progbits,_ZTW9tls_value,comdat
	.hidden	_ZTW9tls_value          // -- Begin function _ZTW9tls_value
	.weak	_ZTW9tls_value
	.p2align	2
	.type	_ZTW9tls_value,@function
_ZTW9tls_value:                         // @_ZTW9tls_value
	.cfi_startproc
// %bb.0:
	stp	x29, x30, [sp, #-16]!   // 16-byte Folded Spill
	mov	x29, sp
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	adrp	x0, :tlsdesc:tls_value
	ldr	x1, [x0, :tlsdesc_lo12:tls_value]
	add	x0, x0, :tlsdesc_lo12:tls_value
	.tlsdesccall tls_value
	blr	x1
	mrs	x8, TPIDR_EL0
	add	x0, x8, x0
	ldp	x29, x30, [sp], #16     // 16-byte Folded Reload
	ret
.Lfunc_end1:
	.size	_ZTW9tls_value, .Lfunc_end1-_ZTW9tls_value
	.cfi_endproc
                                        // -- End function
	.type	tls_value,@object       // @tls_value
	.section	.tbss,"awT",@nobits
	.globl	tls_value
	.p2align	3
tls_value:
	.xword	0                       // 0x0
	.size	tls_value, 8

	.ident	"clang version 10.0.0 "
	.section	".note.GNU-stack","",@progbits
	.addrsig
