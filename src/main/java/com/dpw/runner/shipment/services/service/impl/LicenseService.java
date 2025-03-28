package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.impl.MDMServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.LicenseConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.LicenseRequest;
import com.dpw.runner.shipment.services.commons.responses.LicenseResponse;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import java.util.HashMap;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class LicenseService {


  @Autowired
  private MDMServiceAdapter mdmServiceAdapter;

  public boolean getLicenseByLicenseName(String licenseName) {

      LicenseRequest request = LicenseRequest.builder().licenseName(licenseName).build();
      log.info("Request: {} validateLicense --- for License: {}", LoggerHelper.getRequestIdFromMDC(), licenseName);
      LicenseResponse licenseResponse = null ;

      try {
        licenseResponse = mdmServiceAdapter.validateLicense(CommonRequestModel.buildDependentDataRequest(request));
      }catch (Exception ex){
        log.error("License API Failed for License: {} : {}", licenseName, ex.getMessage());
      }

      return licenseResponse != null && Boolean.TRUE.equals(licenseResponse.getHasValidLicense());
  }

}
