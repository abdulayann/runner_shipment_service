package com.dpw.runner.shipment.services.commons.constants;

import java.util.List;

public class LicenseConstants {
  public static final String DG_AIR = "IATA Dangerous Goods Regulation";
  public static final String OCEAN_DG_PERMISSION = "IMO Dangerous Goods Handling";
  public static final String AIR_SECURITY = "Country Air Cargo Security";

  public static final List<String> LicenseList = List.of(DG_AIR, OCEAN_DG_PERMISSION, AIR_SECURITY);
}
