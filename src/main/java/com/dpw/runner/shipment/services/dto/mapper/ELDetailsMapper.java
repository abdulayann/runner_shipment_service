package com.dpw.runner.shipment.services.dto.mapper;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ELDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.ELDetailsResponse;
import com.dpw.runner.shipment.services.entity.ELDetails;

import java.util.List;
import java.util.stream.Collectors;

public class ELDetailsMapper {
    public static ELDetailsResponse convertEntityToDto(ELDetails elDetails) {
        return ELDetailsResponse.builder()
                .id(elDetails.getId())
                .guid(elDetails.getGuid())
                .shipmentId(elDetails.getShipmentId())
                .elNumber(elDetails.getElNumber())
                .mergeClass(elDetails.getMergeClass())
                .mergePackages(elDetails.getMergePackage())
                .unit(elDetails.getUnit())
                .mergePackageUnit(elDetails.getMergePackageUnit())
                .packages(elDetails.getPackages())
                .partition(elDetails.getPartition())
                .partitionSeqNumber(elDetails.getPartitionSeqNumber())
                .shipmentId(elDetails.getShipmentId())
                .weight(elDetails.getWeight())
                .weightUnit(elDetails.getWeightUnit())
                .build();
    }

    public static ELDetails convertRequestToELDetails(ELDetailsRequest request) {
        return ELDetails.builder()
                .shipmentId(request.getShipmentId())
                .elNumber(request.getElNumber())
                .packages(request.getPackages())
                .unit(request.getUnit())
                .weight(request.getWeight())
                .weightUnit(request.getWeightUnit())
                .mergeClass(request.getMergeClass())
                .mergePackage(request.getMergePackages())
                .mergePackageUnit(request.getMergePackageUnit())
                .partition(request.getPartition())
                .partitionSeqNumber(request.getPartitionSeqNumber())
                .build();
    }

    public static List<IRunnerResponse> convertEntityListToDtoList(final List<ELDetails> lst) {
        return lst.stream()
                .map(item -> ELDetailsMapper.convertEntityToDto(item))
                .collect(Collectors.toList());
    }
}
