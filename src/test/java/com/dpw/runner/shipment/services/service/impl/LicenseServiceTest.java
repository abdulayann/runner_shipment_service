package com.dpw.runner.shipment.services.service.impl;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.adapters.impl.MDMServiceAdapter;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.LicenseResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.ResponseEntity;

@ExtendWith(MockitoExtension.class)
class LicenseServiceTest {

  @Mock
  private MDMServiceAdapter mdmServiceAdapter;

  @InjectMocks
  private LicenseService licenseService;

  @BeforeEach
  void setUp() {
  }

  @Test
  void testGetLicenseByLicenseName_Success() throws RunnerException {
    LicenseResponse responseA = new LicenseResponse();
    responseA.setHasValidLicense(true);

    when(mdmServiceAdapter.validateLicense(any(CommonRequestModel.class)))
        .thenReturn(responseA);

    Boolean result= licenseService.getLicenseByLicenseName("ABC");

    assertNotNull(result);
  }

  @Test
  void testGetLicenseByLicenseName_Failure() throws RunnerException {
    when(mdmServiceAdapter.validateLicense(any(CommonRequestModel.class)))
        .thenReturn(new LicenseResponse());

    Boolean result = licenseService.getLicenseByLicenseName("ABC");

    assertNotNull(result);
  }

  @Test
  void testGetLicenseByLicenseName_Exception() throws RunnerException {
    when(mdmServiceAdapter.validateLicense(any(CommonRequestModel.class)))
        .thenThrow(new ValidationException("ex"));

    assertFalse(licenseService.getLicenseByLicenseName("ASC"));
  }
}
