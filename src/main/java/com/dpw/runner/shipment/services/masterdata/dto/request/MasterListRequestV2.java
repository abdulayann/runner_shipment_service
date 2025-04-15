package com.dpw.runner.shipment.services.masterdata.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class MasterListRequestV2 implements IRunnerRequest {
    private List<MasterListRequest> MasterListRequests = new ArrayList<>();
    private List<String> IncludeCols;
}
