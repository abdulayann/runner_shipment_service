package com.dpw.runner.shipment.services.masterdata.dto.request;

import lombok.*;

import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class MasterListRequestV2 {
    private List<MasterListRequest> MasterListRequests = new ArrayList<>();
    private List<String> IncludeCols;
}
