package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

import java.util.UUID;

@Data
public class HblPartyRequestV2 implements IRunnerRequest {
    private Long id;
    private UUID guid;
    private String type;
    private String orgCode;
    private Integer orgId;
    private String name;
    private String email;
    private String address;
    private String taxId;
}
