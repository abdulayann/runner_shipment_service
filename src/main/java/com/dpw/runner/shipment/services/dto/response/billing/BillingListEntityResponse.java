package com.dpw.runner.shipment.services.dto.response.billing;

import com.dpw.runner.shipment.services.utils.Generated;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Generated
public class BillingListEntityResponse<T> extends BillingBaseResponse {

    private long count;
    private long totalPages;
    private long currentPage;
    private long pageSize;
    private transient List<? extends T> data;
}
