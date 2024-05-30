package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

@Data
public class AwbNotifyPartyInfoV2 implements IRunnerRequest {
    public String entityType;
    public String type;
    public String orgCode;
    //TODO - Please Move this to parties org - Tapan
    public Integer orgId;
    public Long notifyOrgId;
    public String name;
    public String address;
    public Integer addressId;
}
