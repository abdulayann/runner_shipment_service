package com.dpw.runner.shipment.services.notification.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class GetLogsRequest implements IRunnerRequest {
    private List<TagsData> tagsDataList;
    private Integer page;
    private Integer limit;
    private String sort;
    private String order;
    private String type;
}
