package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.*;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
public class AutoAttachConsolidationResponse implements IRunnerResponse {
    private List<IRunnerResponse> consolidationDetailsList;
    private String filteredDetailName;
}
