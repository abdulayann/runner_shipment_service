package com.dpw.runner.shipment.services.aspects.intraBranch;

import com.dpw.runner.shipment.services.dto.request.intraBranch.IntraBranchDto;

import java.util.Objects;

public class IntraBranchContext {
    private IntraBranchContext(){}
    private static final ThreadLocal<IntraBranchDto> currentContext = new ThreadLocal<>();

    public static IntraBranchDto getContext() {
        return Objects.isNull(currentContext.get()) ? IntraBranchDto.builder().build() : currentContext.get();
    }

    public static void setContext(IntraBranchDto context) {
        currentContext.set(context);
    }

    public static void removeContext() {
        currentContext.remove();
    }
}