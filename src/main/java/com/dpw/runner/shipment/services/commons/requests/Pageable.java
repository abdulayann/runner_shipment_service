package com.dpw.runner.shipment.services.commons.requests;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Pageable {
    private int pageNo = 0;
    private int limit = 100;
    private List<FilterCriteria> filterCriteria;
    private SortRequest sortRequest;
}
