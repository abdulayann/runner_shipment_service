package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;
import java.util.UUID;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
public class ConsolidationExcelExportResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private String consolidationNumber;
    private String consolidationType;
    private String transportMode;
    private String shipmentType;
    private Boolean isDomestic;
    private String mawb;
    private String payment;
    private String bookingType;
    private String referenceNumber;
    private String bookingStatus;
    private String bookingNumber;
    private Long containerCount;
    private List<String> houseBills;
    private List<String> shipmentIds;
    private LocalDateTime estimatedTerminalCutoff;
    private LocalDateTime terminalCutoff;
    private LocalDateTime verifiedGrossMassCutoff;
    private LocalDateTime reeferCutoff;
    private LocalDateTime bookingCutoff;
    private LocalDateTime shipInstructionCutoff;
    private LocalDateTime hazardousBookingCutoff;
    private String createdBy;
    private CarrierDetailResponse carrierDetails;
    private Integer tenantId;
    private Long container20Count;
    private Long container40Count;
    private Long container20GPCount;
    private Long container20RECount;
    private Long container40GPCount;
    private Long container40RECount;
    private Set<String> containerNumbers;
}
