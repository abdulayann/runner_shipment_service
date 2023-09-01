package com.dpw.runner.shipment.services.dto.request.npm;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.npm.LoadInfoRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UpdateContractRequest implements IRunnerRequest {
    private String contract_id;
    private String contract_status;
    private String source;
    private String source_type;
    private Boolean is_alteration;
    private List<LoadInfoRequest> loads_info;
}
