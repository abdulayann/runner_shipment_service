package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
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
    boolean fromConsolidation;
    Boolean pushAwbEvent;
    Boolean printWithoutTranslation;
    List<Long> shipmentIds;
    boolean isShipperAndConsignee;
    boolean isSecurityData;
    String transportInstructionId;
    String remarks;
    boolean includeCsdInfo;
    boolean combiLabel;
    PartiesRequest fcrShipper;
    List<Long> packIds;
    String placeOfIssue;
    LocalDateTime dateOfIssue;
    String consolAirline;
    String consolDestinationAirportCode;
    String mawbNumber;
    Integer totalMawbPieces;
    Boolean printCustomLabel;

}
