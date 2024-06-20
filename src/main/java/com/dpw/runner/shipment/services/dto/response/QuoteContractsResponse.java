package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;

import java.util.List;
import java.util.UUID;

public class QuoteContractsResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private String contractId;
    private List<String> containerTypes;
}
