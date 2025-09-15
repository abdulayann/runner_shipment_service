package com.dpw.runner.shipment.services.dto.response.carrierbooking;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.entity.enums.FlowType;
import com.dpw.runner.shipment.services.entity.enums.SourceSystem;
import com.dpw.runner.shipment.services.entity.enums.VerifiedGrossMassStatus;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TransactionHistoryResponse implements IRunnerResponse {

    private Long id;
    private VerifiedGrossMassStatus actionStatus;
    private String actionStatusDescription;
    private FlowType flowType;
    private String description;
    private SourceSystem sourceSystem;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime actualDateTime;
    private String errorMessage;
}
