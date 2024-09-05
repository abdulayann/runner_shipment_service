package com.dpw.runner.shipment.services.dto.response.billing;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.List;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
public class BillingListResponse<T> extends BillingBaseResponse {

    private long count;
    private long totalPages;
    private long currentPage;
    private long pageSize;
    private transient List<? extends T> data;
}
