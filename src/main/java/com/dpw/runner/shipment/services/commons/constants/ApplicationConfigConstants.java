package com.dpw.runner.shipment.services.commons.constants;

public class ApplicationConfigConstants {
  private ApplicationConfigConstants(){}
  public static final String APPLICATION_CONFIG_API_HANDLE = "/api/v2/config";
  public static final String GET_ALL = "/all";
  public static final String REFRESH_JVM_KEYS = "/refreshJvmKeys";
  public static final String CREATE = "/create";
  public static final String UPDATE = "/update";
  public static final String DELETE_BY_ID = "/delete/{id}";

  public static final String EXPORT_EXCEL_LIMIT = "EXPORT_EXCEL_LIMIT";
  public static final String HS_CODE_BATCH_PROCESS_LIMIT = "HS_CODE_BATCH_PROCESS_LIMIT";
}
