package com.dpw.runner.shipment.services.dto.v3.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.response.ArrivalDepartureDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsBaseResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Data
public class ConsolidationDetailsV3Response extends ConsolidationDetailsBaseResponse implements IRunnerResponse {
    private Boolean isDomestic;
    private String serviceLevel;
    private String declarationType;
    private Boolean isLinked;
    private Boolean isCharter;
    private String packageType;
    private String agentReference;
    private String manifestPrint;
    private String printOtherDocs;
    private String awbDims;
    private String releaseType;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime masterBillIssueDate;
    private Boolean override;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime estimatedTerminalCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime terminalCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime verifiedGrossMassCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime reeferCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime bookingCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime shipInstructionCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime hazardousBookingCutoff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime latestFullEquDeliveredToCarrier;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime earliestDropOffFullEquToCarrier;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime earliestEmptyEquPickUp;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime carrierDocCutOff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime cargoReceiptWHCutOff;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime lastFreeDateCutOff;
    private Integer numberOfFreeDaysCutOff;
    private String volumeUtilization;
    private String weightUtilization;
    private Boolean isCargoOnly;
    private String specialInstructions;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime docsClosingTime;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime cargoClosingTime;
    private String mrnNumber;
    private String msnNumber;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime igmFileDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime igmInwardDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime inwardDateAndTime;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime smtpigmDate;
    private Integer original;
    private Integer copy;
    private String doPlaceOfIssue;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime doIssueDate;
    private Long bondedWarehouseId;
    private Long warehouseId;
    private String ediTransactionId;
    private Long triangulationPartner;
    private Boolean intraBranch;
    private Long documentationPartner;
    private Boolean isReceivingAgentFreeTextAddress;
    private String receivingAgentFreeTextAddress;
    private Boolean isSendingAgentFreeTextAddress;
    private String sendingAgentFreeTextAddress;
    private String placeOfIssue;
    private ArrivalDepartureDetailsResponse arrivalDetails;
    private ArrivalDepartureDetailsResponse departureDetails;
    private PartiesResponse sendingAgent;
    private PartiesResponse receivingAgent;
    private PartiesResponse borrowedFrom;
    private PartiesResponse creditor;
    private PartiesResponse coLoadWith;
    private List<PartiesResponse> consolidationAddresses;
    private ContainerSummaryResponse containerSummary;
    private PackSummaryResponse packSummary;
    private Boolean autoUpdateGoodsDesc;
    private PartiesResponse client;
    private PartiesResponse consigner;
    private PartiesResponse consignee;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime cfsCutOffDate;
    private Map<String, Object> masterDataMap;
    private UUID parentGuid;
    private Long parentTenantId;
}
