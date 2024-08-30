package com.dpw.runner.shipment.services.aspects.intraBranch;

import com.dpw.runner.shipment.services.dto.request.intraBranch.InterBranchDto;
import com.dpw.runner.shipment.services.dto.request.intraBranch.InterBranchTenantIdDto;

import java.util.Objects;

public class InterBranchTenantIdContext {
    private InterBranchTenantIdContext() {}

    private static final ThreadLocal<InterBranchTenantIdDto> currentContext = new ThreadLocal<>();

    public static InterBranchTenantIdDto getContext() {
        return Objects.isNull(currentContext.get()) ? null : currentContext.get();
    }

    public static void setContext(InterBranchTenantIdDto context) {
        currentContext.set(context);
    }

    public static void removeContext() {
        currentContext.remove();
    }

}
