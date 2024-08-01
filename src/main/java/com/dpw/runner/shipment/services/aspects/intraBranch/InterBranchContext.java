package com.dpw.runner.shipment.services.aspects.intraBranch;

import com.dpw.runner.shipment.services.dto.request.intraBranch.InterBranchDto;

import java.util.Objects;

public class InterBranchContext {
    private InterBranchContext(){}
    private static final ThreadLocal<InterBranchDto> currentContext = new ThreadLocal<>();

    public static InterBranchDto getContext() {
        return Objects.isNull(currentContext.get()) ? InterBranchDto.builder().build() : currentContext.get();
    }

    public static void setContext(InterBranchDto context) {
        currentContext.set(context);
    }

    public static void removeContext() {
        currentContext.remove();
    }
}