package com.dpw.runner.shipment.services.aspects;

import com.dpw.runner.shipment.services.commons.constants.LicenseConstants;
import com.dpw.runner.shipment.services.service.impl.LicenseService;
import javax.annotation.PostConstruct;
import org.springframework.stereotype.Component;

@Component
public class LicenseContext {

  private static LicenseService licenseService;

  private final LicenseService licenseServiceInstance;

  public LicenseContext(LicenseService licenseServiceInstance) {
    this.licenseServiceInstance = licenseServiceInstance;
  }

  // Initialize the static field after Spring injects the bean
  @PostConstruct
  private void init() {
    licenseService = licenseServiceInstance;
  }

  public static boolean isDgAirLicense() {
      if(licenseService == null){
        throw new IllegalStateException("LicenseContext is not initialized properly by Spring.");
      }
      return licenseService.getLicenseByLicenseName(LicenseConstants.DG_AIR);
  }

  public static boolean isOceanDGLicense() {
    if(licenseService == null){
      throw new IllegalStateException("LicenseContext is not initialized properly by Spring.");
    }

    return licenseService.getLicenseByLicenseName(LicenseConstants.OCEAN_DG_PERMISSION);
  }

  public static boolean isAirSecurityLicense() {
    if(licenseService == null){
      throw new IllegalStateException("LicenseContext is not initialized properly by Spring.");
    }
    return licenseService.getLicenseByLicenseName(LicenseConstants.AIR_SECURITY);
  }
}
