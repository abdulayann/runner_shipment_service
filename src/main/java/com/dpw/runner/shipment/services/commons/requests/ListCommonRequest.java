package com.dpw.runner.shipment.services.commons.requests;

import lombok.*;

import javax.validation.constraints.Size;
import java.util.List;

@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ListCommonRequest implements IRunnerRequest{
    private List<FilterCriteria> filterCriteria;
    private SortRequest sortRequest;
    @Builder.Default
    private Integer pageNo = 1;
    @Builder.Default
    private Integer pageSize = Integer.MAX_VALUE;
    private List<String> includeTbls;
    @Size(min = 3, message = "min size is 3 for containsText search criteria")
    private String containsText;
    private String entityId;
    private List<String> includeColumns;
    private Boolean notificationFlag;
}
