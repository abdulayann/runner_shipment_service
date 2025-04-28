package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
@SuppressWarnings("java:S1948")
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
    boolean printCSD;
    boolean combiLabel;
    PartiesRequest fcrShipper;
    List<Long> packIds;
    String placeOfIssue;
    @ExcludeTimeZone
    LocalDateTime dateOfIssue;
    String consolAirline;
    String destination;
    String mawbNumber;
    Integer totalMawbPieces;
    Boolean printCustomLabel;
    String hawbNumber;
    Integer totalHawbPieces;
    List<HawbInfo> hawbInfo;
    String entityGuid;
    String entityName;

    @Getter
    @Setter
    public static class HawbInfo {
        private String hawbNumber;
        private Integer hawbPieceCount;
    }

}
