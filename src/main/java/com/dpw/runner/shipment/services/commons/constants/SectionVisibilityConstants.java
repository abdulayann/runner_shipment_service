package com.dpw.runner.shipment.services.commons.constants;

public abstract class SectionVisibilityConstants {

  public static final String ALREADY_EXIST = "SectionVisibility with the same tenantId, mode, and direction already exists.";
  public static final String BRANCH_MISMATCHED = "Logged-in with different branch, branch and tenant name is mismatched!!";
  public static final String UPDATE_AUDIT_LOG_ERROR_MESSAGE = "Error while updating audit log for section visibility rule";
  public static final String NOT_FOUND = "SectionVisibility not found with ID: ";
  public static final String ENTITY_DOES_NOT_EXIST = "SectionVisibility with the tenantId, mode, and direction does not exists.";
  public static final String INVALID_IDS = "One or more provided SectionDetail IDs are invalid.";
  public static final String CREATE_AUDIT_LOG_ERROR_MESSAGE = "Error while saving audit log for section visibility rule";
  public static final String ID_CAN_NOT_BE_NULL = "Id can not be null";

  private SectionVisibilityConstants() {
  }

}
