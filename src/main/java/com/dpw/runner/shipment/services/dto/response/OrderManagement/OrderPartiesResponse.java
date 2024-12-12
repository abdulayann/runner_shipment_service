package com.dpw.runner.shipment.services.dto.response.OrderManagement;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class OrderPartiesResponse implements IRunnerResponse {
    private String id;
    private String partyType;
    private String partyCode;
    private String partyName;
    private String addressCode;
    private String reference;
    private String country;
    private Map<String, Object> address;

}
