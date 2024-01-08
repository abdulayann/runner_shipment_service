package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Getter;

@Getter
public class ReportRequest implements IRunnerRequest {
    String reportInfo;
    String reportKey;
    String reportId;
    String printType = null;
    String frontTemplateCode = null;
    String backTemplateCode = null;
    String isWithTax = null;
    Integer copyCountForAWB;
    boolean printForParties;
    boolean printBarcode;
    String printingFor_str = null;
    boolean printIATAChargeCode;
    Boolean displayFreightAmount;
    Boolean displayOtherAmount;
    String inLocalCurrency = null;
    String transportMode = null;
    String multiTemplateCode = null;
    String requestSource = null;
    String noOfCopies = null;
    boolean fromShipment;
}
