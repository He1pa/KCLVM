// Copyright The KCL Authors. All rights reserved.

use super::context::LLVMCodeGenContext;
use crate::codegen::llvm::context::BacktrackKind;
use crate::codegen::traits::BuilderMethods;
use inkwell::values::BasicValueEnum;

impl<'ctx> LLVMCodeGenContext<'ctx> {
    pub(crate) fn update_backtrack_meta(
        &self,
        name: &str,
        schema_value: BasicValueEnum<'ctx>,
    ) -> bool {
        if let Some(backtrack_meta) = self.backtrack_meta.borrow_mut().as_mut() {
            if name == backtrack_meta.target {
                backtrack_meta.count += 1;
                if backtrack_meta.count >= backtrack_meta.level {
                    backtrack_meta.stop = true;
                    self.ret(schema_value);
                    return true;
                }
            }
        }
        false
    }

    #[inline]
    pub(crate) fn is_backtrack_only_if(&self) -> bool {
        if let Some(backtrack_meta) = self.backtrack_meta.borrow_mut().as_ref() {
            matches!(backtrack_meta.kind, BacktrackKind::If)
        } else {
            false
        }
    }

    #[inline]
    pub(crate) fn is_backtrack_only_or_else(&self) -> bool {
        if let Some(backtrack_meta) = self.backtrack_meta.borrow_mut().as_ref() {
            matches!(backtrack_meta.kind, BacktrackKind::OrElse)
        } else {
            false
        }
    }
}
