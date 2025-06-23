package com.dpw.runner.shipment.services.dto.request.billing;

import com.fasterxml.jackson.annotation.JsonInclude;
import java.io.Serializable;
import javax.validation.constraints.Min;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
public class BillingPaginatedRequest implements Serializable {

	@Min(value = 1, message = "Page number must be greater than 0 ")
	private Integer pageNumber = 1;
	@Min(value = 1, message = "Page size must be greater than 1 ")
	private Integer pageSize = 250;
	private String sortField;
	private String sortDirection;
	private boolean isPaginationRequired = true;
}
