package com.dpw.runner.shipment.services.commons.requests;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import lombok.*;

import javax.validation.constraints.Min;
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
}
