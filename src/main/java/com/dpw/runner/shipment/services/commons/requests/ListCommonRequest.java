package com.dpw.runner.shipment.services.commons.requests;

import lombok.*;

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
    private String containsText;
    private List<String> includeColumns;
    private Boolean notificationFlag;
}
