package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.request.ListCousinBranchesForEtRequest;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.service.interfaces.IMasterDataService;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@RestController
@RequestMapping(MasterDataConstants.MASTER_DATA_API_HANDLE)
@Slf4j
public class MasterDataController {

    private final IMasterDataService masterDataService;

    @Autowired
    public MasterDataController(IMasterDataService masterDataService) {
        this.masterDataService = masterDataService;
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> create(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.create(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> update(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.update(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.list(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.CARRIER + ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createCarrier(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.createCarrier(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.CARRIER + ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> updateCarrier(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.updateCarrier(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.CARRIER + ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> carrierList(@RequestBody @Valid CarrierListObject request) {
        String responseMsg;
        try {
            return masterDataService.listCarrier(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.CONTAINER_TYPE + ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createContainerType(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.createContainerType(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.CONTAINER_TYPE + ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> updateContainerType(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.updateContainerType(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.CONTAINER_TYPE + ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> listContainerType(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.listContainerType(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.VESSEL + ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createVessel(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.createVessel(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.VESSEL + ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> updateVessel(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.updateVessel(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.VESSEL + ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> listVessel(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.listVessel(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.ROUTING_MASTER + ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createRoutingMaster(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.createRoutingMaster(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.ROUTING_MASTER + ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> updateRoutingMaster(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.updateRoutingMaster(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.ROUTING_MASTER + ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> listRoutingMaster(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.listRoutingMaster(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.CURRENCIES + ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createCurrencies(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.createCurrencies(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.CURRENCIES + ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> updateCurrencies(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.updateCurrencies(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.CURRENCIES + ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> listCurrencies(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.listCurrencies(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.DANGEROUS_GOOD + ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createDangerousGood(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.createDangerousGood(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.DANGEROUS_GOOD + ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> updateDangerousGood(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.updateDangerousGood(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.DANGEROUS_GOOD + ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> listDangerousGood(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.listDangerousGood(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.WAREHOUSE + ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createWarehouse(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.createWarehouse(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.WAREHOUSE + ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> updateWarehouse(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.updateWarehouse(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.WAREHOUSE + ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> listWarehouse(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.listWarehouse(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.PORTS + ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createPorts(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.createPorts(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.PORTS + ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> updatePorts(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.updatePorts(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.PORTS + ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> listPorts(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.listPorts(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.COMMODITY + ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createCommodity(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.createCommodity(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.COMMODITY + ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> updateCommodity(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.updateCommodity(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.COMMODITY + ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> listCommodity(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.listCommodity(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.SALES_AGENT + ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createSalesAgent(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.createSalesAgent(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.SALES_AGENT + ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> updateSalesAgent(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.updateSalesAgent(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.SALES_AGENT + ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> listSalesAgent(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.listSalesAgent(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.UNLOCATION + ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createUnlocation(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.createUnlocation(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.UNLOCATION + ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> updateUnlocation(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.updateUnlocation(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.UNLOCATION + ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> listUnlocation(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.listUnlocation(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.UNLOCATION + ApiConstants.STATE_BASED_LIST)
    public ResponseEntity<IRunnerResponse> stateBasedList(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.stateBasedList(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.ORGANIZATION + ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createOrganization(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.createOrganization(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.ORGANIZATION + ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> updateOrganization(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.updateOrganization(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.ORGANIZATION + ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> listOrgnization(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.listOrganization(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.USER + ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> userList(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.listUsers(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.GRID_COLOR_CODE + ApiConstants.API_CREATE)
    public ResponseEntity<IRunnerResponse> createGridColorCode(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.createGridColorCode(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.GRID_COLOR_CODE + ApiConstants.API_UPDATE)
    public ResponseEntity<IRunnerResponse> updateGridColorCOde(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.updateGridColorCode(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.GRID_COLOR_CODE + ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> gridColorList(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.listGridColorCode(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.TENANT + MasterDataConstants.LIST_COUSIN_BRANCH)
    public ResponseEntity<IRunnerResponse> listCousinBranch(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.listCousinBranches(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.TENANT + MasterDataConstants.LIST_COUSIN_BRANCH_WITHOUT_CURRENT)
    public ResponseEntity<IRunnerResponse> listCousinBranchWithoutCurrent(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.listCousinBranchesWithoutCurrent(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.SCHEDULE + ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> scheduleList(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.listSailingSchedule(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.SCHEDULE + MasterDataConstants.IMPORT_SCHEDULE)
    public ResponseEntity<IRunnerResponse> importSchedule(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.importSailingSchedules(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.SCHEDULE + MasterDataConstants.IMPORT_FLIGHT_SCHEDULE)
    public ResponseEntity<IRunnerResponse> importFlightSchedule(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.importFlightSchedules(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.SCHEDULE + MasterDataConstants.FETCH_FLIGHT_STATUS)
    public ResponseEntity<IRunnerResponse> fetchFlightStatus(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.fetchFlightStatus(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.TENANT + MasterDataConstants.TENANT_NAME_BY_ID)
    public ResponseEntity<IRunnerResponse> tenantNameByid(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.tenantNameByTenantId(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.ORGANIZATION + MasterDataConstants.ADDRESS_LIST)
    public ResponseEntity<IRunnerResponse> addressList(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.addressList(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.UNLOCATION + MasterDataConstants.UNLOCATION_ORIGIN_DESTINATION)
    public ResponseEntity<IRunnerResponse> fetchUnlocationOriginAndDestinationList(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.fetchUnlocationOriginAndDestinationList(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.UNLOCATION + MasterDataConstants.UNLOCATION_WITH_TRANSPORT_MODE)
    public ResponseEntity<IRunnerResponse> fetchListUnlocationTransportModeBased(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.fetchListUnlocationTransportModeBased(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.ACTIVITY_MASTER + ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> fetchActivityMaster(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.fetchActivityMaster(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.TENANT_SETTINGS + MasterDataConstants.RETRIEVE_TENANT_SETTINGS)
    public ResponseEntity<IRunnerResponse> tenantSettings() {
        String responseMsg;
        try {
            return masterDataService.retrieveTenantSettings();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.OWN_TYPE)
    public ResponseEntity<IRunnerResponse> listOwnType(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.listOwnType(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.CARRIER_FILTER_LIST)
    public ResponseEntity<IRunnerResponse> carrierFilterList(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.listCarrierFilter(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.LIST_MAIN_PAGE_TEMPLATE)
    public ResponseEntity<IRunnerResponse> getMainPageTemplate(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.fetchGetTemplateMainPage(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.LIST_ROLES)
    public ResponseEntity<IRunnerResponse> listRole(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.listRoles(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.LIST_CHARGE_TYPES)
    public ResponseEntity<IRunnerResponse> fetchChargeTypes(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.fetchChargeTypes(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @GetMapping(MasterDataConstants.DEFAULT_ORG)
    public ResponseEntity<IRunnerResponse> getDefaultOrg() {
        String responseMsg;
        try {
            return masterDataService.getDefaultOrg(CommonRequestModel.buildRequest());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.LIST_ORGS)
    public ResponseEntity<IRunnerResponse> listOrgs(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.listOrgs(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {@ApiResponse(code = 200, message = AwbConstants.MASTER_DATA_RETRIEVE_SUCCESS)})
    @PostMapping(MasterDataConstants.MULTIPLE_MASTER_DATA)
    public ResponseEntity<IRunnerResponse> fetchMultipleMasterData(@RequestBody MasterListRequestV2 request) {
        String responseMsg;
        try {
            return masterDataService.fetchMultipleMasterData(CommonRequestModel.buildRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.TENANT_DATA_RETRIEVAL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(ApiConstants.LIST_BRANCHES_BY_DEFAULT_ORG_AND_ADDRESS)
    public ResponseEntity<IRunnerResponse> listBranchesByDefaultOrgAndAddress(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return masterDataService.listBranchesByDefaultOrgAndAddress(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.TENANT + MasterDataConstants.LIST_COUSIN_BRANCH_FOR_NTE_REASSIGN)
    public ResponseEntity<IRunnerResponse> listCousinBranchForNTEReassign(@RequestBody @Valid ListCousinBranchesForEtRequest request) {
        String responseMsg;
        try {
            request.setIsReassign(true);
            return masterDataService.listCousinBranchForEt(request);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL, response = DependentServiceResponse.class),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.TENANT + MasterDataConstants.LIST_COUSIN_BRANCH_FOR_ET)
    public ResponseEntity<IRunnerResponse> listCousinBranchForEt(@RequestBody @Valid ListCousinBranchesForEtRequest request) {
        String responseMsg;
        try {
            return masterDataService.listCousinBranchForEt(request);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
}
