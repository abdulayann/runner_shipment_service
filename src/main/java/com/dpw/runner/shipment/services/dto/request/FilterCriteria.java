package com.dpw.runner.shipment.services.dto.request;

import lombok.*;

import java.util.List;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class FilterCriteria {

    private Criteria criteria;
    private String logicOperator;
    private List<FilterCriteria> innerFilter;

}
