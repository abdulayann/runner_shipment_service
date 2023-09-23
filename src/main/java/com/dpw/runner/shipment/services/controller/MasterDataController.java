package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IMasterDataService;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

@RestController
@RequestMapping(MasterDataConstants.MASTER_DATA_API_HANDLE)
@Slf4j
public class MasterDataController {

    @Autowired
    private IMasterDataService masterDataService;

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(ApiConstants.API_CREATE)
    public ResponseEntity<DependentServiceResponse> create(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.create(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(ApiConstants.API_UPDATE)
    public ResponseEntity<DependentServiceResponse> update(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.update(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<DependentServiceResponse> list(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.list(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.CARRIER + ApiConstants.API_CREATE)
    public ResponseEntity<DependentServiceResponse> createCarrier(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.createCarrier(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.CARRIER + ApiConstants.API_UPDATE)
    public ResponseEntity<DependentServiceResponse> updateCarrier(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.updateCarrier(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.CARRIER + ApiConstants.API_LIST)
    public ResponseEntity<DependentServiceResponse> carrierList(@RequestBody @Valid CarrierListObject request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.listCarrier(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.CONTAINER_TYPE + ApiConstants.API_CREATE)
    public ResponseEntity<DependentServiceResponse> createContainerType(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.createContainerType(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.CONTAINER_TYPE + ApiConstants.API_UPDATE)
    public ResponseEntity<DependentServiceResponse> updateContainerType(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.updateContainerType(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.CONTAINER_TYPE + ApiConstants.API_LIST)
    public ResponseEntity<DependentServiceResponse> listContainerType(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.listContainerType(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.VESSEL + ApiConstants.API_CREATE)
    public ResponseEntity<DependentServiceResponse> createVessel(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.createVessel(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.VESSEL + ApiConstants.API_UPDATE)
    public ResponseEntity<DependentServiceResponse> updateVessel(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.updateVessel(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.VESSEL + ApiConstants.API_LIST)
    public ResponseEntity<DependentServiceResponse> listVessel(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.listVessel(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.ROUTING_MASTER + ApiConstants.API_CREATE)
    public ResponseEntity<DependentServiceResponse> createRoutingMaster(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.createRoutingMaster(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.ROUTING_MASTER + ApiConstants.API_UPDATE)
    public ResponseEntity<DependentServiceResponse> updateRoutingMaster(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.updateRoutingMaster(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.ROUTING_MASTER + ApiConstants.API_LIST)
    public ResponseEntity<DependentServiceResponse> listRoutingMaster(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.listRoutingMaster(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.CURRENCIES + ApiConstants.API_CREATE)
    public ResponseEntity<DependentServiceResponse> createCurrencies(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.createCurrencies(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.CURRENCIES + ApiConstants.API_UPDATE)
    public ResponseEntity<DependentServiceResponse> updateCurrencies(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.updateCurrencies(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.CURRENCIES + ApiConstants.API_LIST)
    public ResponseEntity<DependentServiceResponse> listCurrencies(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.listCurrencies(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.DANGEROUS_GOOD + ApiConstants.API_CREATE)
    public ResponseEntity<DependentServiceResponse> createDangerousGood(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.createDangerousGood(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.DANGEROUS_GOOD + ApiConstants.API_UPDATE)
    public ResponseEntity<DependentServiceResponse> updateDangerousGood(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.updateDangerousGood(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.DANGEROUS_GOOD + ApiConstants.API_LIST)
    public ResponseEntity<DependentServiceResponse> listDangerousGood(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.listDangerousGood(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.WAREHOUSE + ApiConstants.API_CREATE)
    public ResponseEntity<DependentServiceResponse> createWarehouse(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.createWarehouse(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.WAREHOUSE + ApiConstants.API_UPDATE)
    public ResponseEntity<DependentServiceResponse> updateWarehouse(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.updateWarehouse(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.WAREHOUSE + ApiConstants.API_LIST)
    public ResponseEntity<DependentServiceResponse> listWarehouse(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.listWarehouse(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.PORTS + ApiConstants.API_CREATE)
    public ResponseEntity<DependentServiceResponse> createPorts(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.createPorts(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.PORTS + ApiConstants.API_UPDATE)
    public ResponseEntity<DependentServiceResponse> updatePorts(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.updatePorts(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.PORTS + ApiConstants.API_LIST)
    public ResponseEntity<DependentServiceResponse> listPorts(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.listPorts(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.COMMODITY + ApiConstants.API_CREATE)
    public ResponseEntity<DependentServiceResponse> createCommodity(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.createCommodity(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.COMMODITY + ApiConstants.API_UPDATE)
    public ResponseEntity<DependentServiceResponse> updateCommodity(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.updateCommodity(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.COMMODITY + ApiConstants.API_LIST)
    public ResponseEntity<DependentServiceResponse> listCommodity(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.listCommodity(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.SALES_AGENT + ApiConstants.API_CREATE)
    public ResponseEntity<DependentServiceResponse> createSalesAgent(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.createSalesAgent(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.SALES_AGENT + ApiConstants.API_UPDATE)
    public ResponseEntity<DependentServiceResponse> updateSalesAgent(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.updateSalesAgent(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.SALES_AGENT + ApiConstants.API_LIST)
    public ResponseEntity<DependentServiceResponse> listSalesAgent(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.listSalesAgent(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.UNLOCATION + ApiConstants.API_CREATE)
    public ResponseEntity<DependentServiceResponse> createUnlocation(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.createUnlocation(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.UNLOCATION + ApiConstants.API_UPDATE)
    public ResponseEntity<DependentServiceResponse> updateUnlocation(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.updateUnlocation(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.UNLOCATION + ApiConstants.API_LIST)
    public ResponseEntity<DependentServiceResponse> listUnlocation(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.listUnlocation(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.ORGANIZATION + ApiConstants.API_CREATE)
    public ResponseEntity<DependentServiceResponse> createOrganization(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.createOrganization(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.ORGANIZATION + ApiConstants.API_UPDATE)
    public ResponseEntity<DependentServiceResponse> updateOrganization(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.updateOrganization(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.ORGANIZATION + ApiConstants.API_LIST)
    public ResponseEntity<DependentServiceResponse> listOrgnization(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.listOrganization(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.USER + ApiConstants.API_LIST)
    public ResponseEntity<DependentServiceResponse> userList(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.listUsers(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.CREATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.GRID_COLOR_CODE + ApiConstants.API_CREATE)
    public ResponseEntity<DependentServiceResponse> createGridColorCode(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.createGridColorCode(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.UPDATE_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.GRID_COLOR_CODE + ApiConstants.API_UPDATE)
    public ResponseEntity<DependentServiceResponse> updateGridColorCOde(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.updateGridColorCode(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.GRID_COLOR_CODE + ApiConstants.API_LIST)
    public ResponseEntity<DependentServiceResponse> gridColorList(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.listGridColorCode(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.TENANT + MasterDataConstants.LIST_COUSIN_BRANCH)
    public ResponseEntity<DependentServiceResponse> listCousinBranch(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.listCousinBranches(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.TENANT + MasterDataConstants.LIST_COUSIN_BRANCH_WITHOUT_CURRENT)
    public ResponseEntity<DependentServiceResponse> listCousinBranchWithoutCurrent(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.listCousinBranchesWithoutCurrent(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.SCHEDULE + ApiConstants.API_LIST)
    public ResponseEntity<DependentServiceResponse> scheduleList(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.listSailingSchedule(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.SCHEDULE + MasterDataConstants.IMPORT_SCHEDULE)
    public ResponseEntity<DependentServiceResponse> importSchedule(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.importSailingSchedules(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.SCHEDULE + MasterDataConstants.IMPORT_FLIGHT_SCHEDULE)
    public ResponseEntity<DependentServiceResponse> importFlightSchedule(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.importFlightSchedules(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.SCHEDULE + MasterDataConstants.FETCH_FLIGHT_STATUS)
    public ResponseEntity<DependentServiceResponse> fetchFlightStatus(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.fetchFlightStatus(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.TENANT + MasterDataConstants.TENANT_NAME_BY_ID)
    public ResponseEntity<DependentServiceResponse> tenantNameByid(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.tenantNameByTenantId(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.ORGANIZATION + MasterDataConstants.ADDRESS_LIST)
    public ResponseEntity<DependentServiceResponse> addressList(@RequestBody @Valid Object request) {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.addressList(CommonRequestModel.buildDependentDataRequest(request));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }

    @ApiResponses(value = {
            @ApiResponse(code = 200, message = ShipmentConstants.LIST_SUCCESSFUL),
            @ApiResponse(code = 404, message = Constants.NO_DATA, response = DependentServiceResponse.class)
    })
    @PostMapping(MasterDataConstants.TENANT_SETTINGS + MasterDataConstants.RETRIEVE_TENANT_SETTINGS)
    public ResponseEntity<DependentServiceResponse> tenantSettings() {
        String responseMsg;
        try {
            return (ResponseEntity<DependentServiceResponse>) masterDataService.retrieveTenantSettings();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
        return (ResponseEntity<DependentServiceResponse>) ResponseHelper.buildFailedResponse(responseMsg);
    }
}
