package com.dpw.runner.booking.services.commons.requests;

import lombok.*;

import java.io.Serializable;
import java.util.List;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class FilterCriteria implements Serializable {

    private Criteria criteria;
    private String logicOperator;
    private List<FilterCriteria> innerFilter;

}
