package com.dpw.runner.shipment.services.aspects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.dpw.runner.shipment.services.service.impl.LicenseService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
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
    assertEquals("LicenseContext is not initialized properly by Spring.", exception.getMessage());
  }

  @Test
  void testIsOceanDGLicense_UninitializedContext() {
    Exception exception = assertThrows(IllegalStateException.class, LicenseContext::isOceanDGLicense);
    assertEquals("LicenseContext is not initialized properly by Spring.", exception.getMessage());
  }

  @Test
  void testIsAirSecurityLicense_UninitializedContext() {
    Exception exception = assertThrows(IllegalStateException.class, LicenseContext::isAirSecurityLicense);
    assertEquals("LicenseContext is not initialized properly by Spring.", exception.getMessage());
  }
}
