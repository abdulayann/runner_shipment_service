package com.dpw.runner.shipment.services.commons.constants;

public abstract class SectionDetailsConstant {

  public static final String SECTION_ALREADY_EXIST = "Section code already exists: ";
  public static final String ATLEAST_ONE_SECTION_FIELD_REQUIRED = "At least one SectionField ID is required!";
  public static final String SECTION_FIELD_INVALID_IDS = "One or more SectionField IDs are invalid!";
  public static final String ID_CAN_NOT_NULL = "Id can not be null";
  public static final String NOT_FOUND = "SectionDetails not found!";
  public static final String CAN_NOT_DELETE_LINK_EXIST = "Cannot delete. SectionDetails is linked to SectionVisibility.";

  private SectionDetailsConstant() {
  }
}
