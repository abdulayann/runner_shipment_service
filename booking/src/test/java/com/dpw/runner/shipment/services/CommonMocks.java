package com.dpw.runner.shipment.services;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import org.mockito.Mock;

import static org.mockito.Mockito.when;

public class CommonMocks {

    @Mock
    public CommonUtils commonUtils;

    public void mockTenantSettings() {
        when(commonUtils.getCurrentTenantSettings()).thenReturn(TenantSettingsDetailsContext.getCurrentTenantSettings());
    }
}
