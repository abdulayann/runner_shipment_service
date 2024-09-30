package com.dpw.runner.booking.services;

import com.dpw.runner.booking.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.booking.services.utils.CommonUtils;
import org.mockito.Mock;

import static org.mockito.Mockito.when;

public class CommonMocks {

    @Mock
    public CommonUtils commonUtils;

    public void mockTenantSettings() {
        when(commonUtils.getCurrentTenantSettings()).thenReturn(TenantSettingsDetailsContext.getCurrentTenantSettings());
    }
}
