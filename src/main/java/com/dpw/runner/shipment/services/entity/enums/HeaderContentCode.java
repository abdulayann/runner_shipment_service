package com.dpw.runner.shipment.services.entity.enums;

public enum HeaderContentCode {
  A("AS_AGREED"),
  C("CONSOLIDATION"),
  L("LETTER_OF_CREDIT");

  private final String code;

  HeaderContentCode(String code) {
    this.code = code;
  }

  public String getCode() {
    return code;
  }

}
