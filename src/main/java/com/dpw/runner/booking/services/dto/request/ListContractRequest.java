package com.dpw.runner.booking.services.dto.request;

import com.dpw.runner.booking.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.time.LocalDate;
import java.util.List;

@Getter
@Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ListContractRequest implements IRunnerRequest {
    private String customer_org_id;
    private String org_role;
    private List<String> filter_contract_states;
    private String filter_contract_id;
    private Long filter_per_page_records;
    private LocalDate filter_start_date;
    private List<String> filter_attributes;
}
