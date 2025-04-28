package com.dpw.runner.shipment.services.commons.requests;

import lombok.*;

import java.io.Serializable;
import java.util.List;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString
@SuppressWarnings("java:S1948")
public class FilterCriteria implements Serializable {

    private Criteria criteria;
    private String logicOperator;
    private List<FilterCriteria> innerFilter;

}
