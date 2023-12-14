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
    @Min(value = 1, message = Constants.PageNumberError)
    private int pageNo;
    private int pageSize;
    private List<String> includeTbls;
    private String containsText;
    private List<String> includeColumns;
}
