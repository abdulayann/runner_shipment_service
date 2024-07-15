package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.utils.Generated;
import lombok.Getter;
import lombok.ToString;

@ToString
@Getter
@Generated
public class EntityIdAndTypeRequest implements IRunnerRequest {
    private Long entityId;
    private String entityType;
}
