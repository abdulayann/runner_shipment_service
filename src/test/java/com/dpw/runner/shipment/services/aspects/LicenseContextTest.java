package com.dpw.runner.shipment.services.aspects;

import static com.dpw.runner.shipment.services.commons.constants.LicenseConstants.LICENSE_CONTEXT_NOT_INITALIZED;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.commons.constants.LicenseConstants;
import com.dpw.runner.shipment.services.service.impl.LicenseService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class LicenseContextTest {

  @Mock
  private LicenseService licenseService;

  private LicenseContext licenseContext;

  @BeforeEach
  void setUp() {
    MockitoAnnotations.openMocks(this);
    licenseContext = new LicenseContext(licenseService);
  }

  @Test
  void testIsDgAirLicense_UninitializedContext() {
    Exception exception = assertThrows(IllegalStateException.class, LicenseContext::isDgAirLicense);
    assertEquals(LICENSE_CONTEXT_NOT_INITALIZED, exception.getMessage());
  }

  @Test
  void testIsOceanDGLicense_UninitializedContext() {
    Exception exception = assertThrows(IllegalStateException.class, LicenseContext::isOceanDGLicense);
    assertEquals(LICENSE_CONTEXT_NOT_INITALIZED, exception.getMessage());
  }

  @Test
  void testIsAirSecurityLicense_UninitializedContext() {
    Exception exception = assertThrows(IllegalStateException.class, LicenseContext::isAirSecurityLicense);
    assertEquals(LICENSE_CONTEXT_NOT_INITALIZED, exception.getMessage());
  }

}
