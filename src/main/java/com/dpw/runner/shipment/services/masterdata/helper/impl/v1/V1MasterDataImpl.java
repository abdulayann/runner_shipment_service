package com.dpw.runner.shipment.services.masterdata.helper.impl.v1;

import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.helper.IMasterDataService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

public class V1MasterDataImpl implements IMasterDataService {

    @Autowired
    private IV1Service v1Service;

    @Override
    public DependentServiceResponse fetchMasterData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchMasterData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();
    }

    @Override
    public DependentServiceResponse createMasterData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createMasterData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();
    }

    @Override
    public DependentServiceResponse updateMasterData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateMasterData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();
    }

    @Override
    public DependentServiceResponse fetchCarrierMasterData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchCarrierMasterData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();
    }

    @Override
    public DependentServiceResponse createCarrierMasterData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createCarrierMasterData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();
    }

    @Override
    public DependentServiceResponse updateCarrierMasterData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateCarrierMasterData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();
    }

    @Override
    public DependentServiceResponse fetchContainerTypeData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchContainerTypeData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse createContainerTypeData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createContainerTypeData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse updateContainerTypeData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateContainerTypeData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse fetchVesselData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchVesselData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse createVesselData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createVesselData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse updateVesselData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateVesselData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse fetchRoutingMasterData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchRoutingMasterData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse createRoutingMasterData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createRoutingMasterData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse updateRoutingMasterData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateRoutingMasterData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse fetchCurrenciesData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchCurrenciesData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse createCurrenciesData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createCurrenciesData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse updateCurrenciesData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateCurrenciesData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse fetchDangerousGoodData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchDangerousGoodData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse createDangerousGoodData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createDangerousGoodData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse updateDangerousGoodData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateDangerousGoodData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse fetchWarehouseData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchWarehouseData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse createWarehouseData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createWarehouseData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse updateWarehouseData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateWarehouseData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse fetchPortsData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchPortsData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse createPortsData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createPortsData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse updatePortsData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updatePortsData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse fetchCommodityData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchCommodityData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse createCommodityData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createCommodityData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse updateCommodityData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateCommodityData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse fetchSalesAgentData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchSalesAgentData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse createSalesAgentData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createSalesAgentData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse updateSalesAgentData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateSalesAgentData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();    }

    @Override
    public DependentServiceResponse fetchOrganizationData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchOrganization(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();
    }

    @Override
    public DependentServiceResponse createOrganizationData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createOrganizationData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();
    }

    @Override
    public DependentServiceResponse updateOrganizationData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateOrganizationData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();
    }

    @Override
    public DependentServiceResponse fetchUnlocationData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchUnlocation(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();
    }

    @Override
    public DependentServiceResponse createUnlocationData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createUnlocationData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();
    }

    @Override
    public DependentServiceResponse updateUnlocationData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateUnlocationData( request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).numberOfRecords(v1DataResponse.take).totalPages(v1DataResponse.totalCount).build();
    }

    @Override
    public List<MasterData> fetchByType(MasterDataType masterDataType) {
        return null;
    }
}
