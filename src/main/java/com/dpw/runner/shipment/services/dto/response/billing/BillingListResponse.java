package com.dpw.runner.shipment.services.dto.response.billing;

import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

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
