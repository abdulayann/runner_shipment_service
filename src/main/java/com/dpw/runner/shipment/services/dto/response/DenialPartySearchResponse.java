package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.UUID;

public class DenialPartySearchResponse implements IRunnerResponse {
    List<DenialPartySearchChild> entity;
}

