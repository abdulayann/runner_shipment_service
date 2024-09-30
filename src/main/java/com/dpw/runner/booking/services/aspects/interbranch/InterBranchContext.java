package com.dpw.runner.booking.services.aspects.interbranch;

import com.dpw.runner.booking.services.dto.request.intraBranch.InterBranchDto;

import java.util.Objects;

public class InterBranchContext {
    private InterBranchContext(){}
    private static final ThreadLocal<InterBranchDto> currentContext = new ThreadLocal<>();

    public static InterBranchDto getContext() {
        return Objects.isNull(currentContext.get()) ? null : currentContext.get();
    }

    public static void setContext(InterBranchDto context) {
        currentContext.set(context);
    }

    public static void removeContext() {
        currentContext.remove();
    }
}