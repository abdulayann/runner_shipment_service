package com.dpw.runner.shipment.services.commons.requests;

import lombok.Getter;
import lombok.ToString;

import java.util.List;

@Getter
@ToString
public class ListCommonRequest implements IRunnerRequest{
    private List<Criteria> criteriaRequests;
    private List<Criteria> orCriteriaRequests;
    private SortRequest sortRequest;
    private int pageNo;
    private int count;
}
