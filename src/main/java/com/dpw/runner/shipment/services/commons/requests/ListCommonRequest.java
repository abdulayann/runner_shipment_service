package com.dpw.runner.shipment.services.commons.requests;

import lombok.Builder;
import lombok.Getter;
import lombok.ToString;

import java.util.List;

@Getter
@ToString
public class ListCommonRequest implements IRunnerRequest{
    private List<FilterCriteria> filterCriteria;
    private SortRequest sortRequest;
    private int pageNo;
    private int limit;
}
