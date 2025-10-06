package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Set;

@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Data
public class ConsolidationListResponse extends ConsolidationDetailsBaseResponse implements IRunnerResponse {
    private Boolean isDomestic;
    private String serviceLevel;
    private String firstLoad;
    private String lastDischarge;
    private String bookingType;
    private String declarationType;
    private String deliveryMode;
    private Boolean isLinked;
    private Boolean isCharter;
    private String packageType;
    private String agentReference;
    private String manifestPrint;
    private String printOtherDocs;
    private String awbDims;
    private String releaseType;
    private LocalDateTime masterBillIssueDate;
    private Boolean override;
    private LocalDateTime estimatedTerminalCutoff;
    private LocalDateTime bookingCutoff;
    private String volumeUtilization;
    private String weightUtilization;
    private Boolean isCargoOnly;
    private String specialInstructions;
    private LocalDateTime docsClosingTime;
    private LocalDateTime cargoClosingTime;
    private String mrnNumber;
    private String msnNumber;
    private Integer original;
    private Integer copy;
    private String doPlaceOfIssue;
    private LocalDateTime doIssueDate;
    private Long bondedWarehouseId;
    private Long warehouseId;
    private String ediTransactionId;
    private Long triangulationPartner;
    private boolean intraBranch;
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
    private List<String> houseBills;
    private List<String> shipmentIds;
    private BigDecimal teuCount;
    private Long container20Count;
    private Long container40Count;
    private Long container20GPCount;
    private Long container20RECount;
    private Long container40GPCount;
    private Long container40RECount;
    private Long containerCount;
    private Set<String> containerNumbers;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime cfsCutOffDate;
    private Boolean openForAttachment;
    private Boolean interBranchConsole;
    private LocalDateTime createdAt;
    private String requestedType;
    private String requestedBy;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime requestedOn;
    private Map<String, String> tenantMasterData;
    private Boolean reefer = false;
    private String incoterms;
}
