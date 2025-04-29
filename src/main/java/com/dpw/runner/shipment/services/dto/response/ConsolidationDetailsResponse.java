package com.dpw.runner.shipment.services.dto.response;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class ConsolidationDetailsResponse extends ConsolidationDetailsV3Response {
    private List<PackingResponse> packingList;
    private List<RoutingsResponse> routingsList;
    private List<ContainerResponse> containersList;
    private List<TruckDriverDetailsResponse> truckDriverDetails;
    private List<JobResponse> jobsList;
    private List<EventsResponse> eventsList;
    private List<FileRepoResponse> fileRepoList;
}
