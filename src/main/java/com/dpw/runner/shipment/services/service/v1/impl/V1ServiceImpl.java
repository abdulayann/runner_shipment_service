package com.dpw.runner.shipment.services.service.v1.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.v1.request.CreateConsolidationTaskRequest;
import com.dpw.runner.shipment.services.dto.v1.request.CreateShipmentTaskRequest;
import com.dpw.runner.shipment.services.dto.v1.response.SendEntityResponse;
import com.dpw.runner.shipment.services.dto.v1.response.TenantIdResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1ShipmentCreationResponse;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.exception.exceptions.UnAuthorizedException;
import com.dpw.runner.shipment.services.exception.exceptions.V1ServiceException;
import com.dpw.runner.shipment.services.exception.response.V1ErrorResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.client.RestTemplate;


import static com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil.createBookingRequestForV1;

@Service
//@EnableAsync
public class V1ServiceImpl implements IV1Service {

    private static final Logger log = LoggerFactory.getLogger(V1ServiceImpl.class);

    @Autowired
    private RestTemplate restTemplate;

    @Value("${v1service.url.base}${v1service.url.customerBooking}")
    private String CUSTOMER_BOOKING_URL;

    @Value("${v1service.url.base}${v1service.url.masterData}")
    private String MASTER_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.masterDataCreate}")
    private String MASTER_DATA_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.masterDataUpdate}")
    private String MASTER_DATA_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.carrierData}")
    private String CARRIER_MASTER_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.carrierOrgRefFilteredData}")
    private String CARRIER_MASTER_DATA_ORG_REF_FILTER_URL;

    @Value("${v1service.url.base}${v1service.url.carrierDataCreate}")
    private String CARRIER_MASTER_DATA_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.carrierDataUpdate}")
    private String CARRIER_MASTER_DATA_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.containerTypeData}")
    private String CONTAINER_TYPE_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.containerTypeCreate}")
    private String CONTAINER_TYPE_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.containerTypeUpdate}")
    private String CONTAINER_TYPE_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.vesselData}")
    private String VESSEL_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.vesselCreate}")
    private String VESSEL_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.vesselUpdate}")
    private String VESSEL_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.routingMasterData}")
    private String ROUTING_MASTER_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.routingMasterCreate}")
    private String ROUTING_MASTER_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.routingMasterUpdate}")
    private String ROUTING_MASTER_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.currenciesData}")
    private String CURRENCIES_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.currenciesCreate}")
    private String CURRENCIES_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.currenciesUpdate}")
    private String CURRENCIES_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.dangerousGoodData}")
    private String DANGEROUS_GOOD_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.dangerousGoodCreate}")
    private String DANGEROUS_GOOD_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.dangerousGoodUpdate}")
    private String DANGEROUS_GOOD_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.warehouseData}")
    private String WAREHOUSE_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.warehouseCreate}")
    private String WAREHOUSE_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.warehouseUpdate}")
    private String WAREHOUSE_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.portsData}")
    private String PORTS_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.portsCreate}")
    private String PORTS_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.portsUpdate}")
    private String PORTS_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.commodityData}")
    private String COMMODITY_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.commodityCreate}")
    private String COMMODITY_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.commodityUpdate}")
    private String COMMODITY_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.salesAgentData}")
    private String SALES_AGENT_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.salesAgentCreate}")
    private String SALES_AGENT_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.salesAgentUpdate}")
    private String SALES_AGENT_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.unlocation}")
    private String UNLOCATION_URL;

    @Value("${v1service.url.base}${v1service.url.organization}")
    private String ORGANIZATION_API;

    @Value("${v1service.url.base}${v1service.url.unlocationCreate}")
    private String UNLOCATION_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.unlocationUpdate}")
    private String UNLOCATION_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.organizationCreate}")
    private String ORGANIZATION_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.organizationUpdate}")
    private String ORGANIZATION_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.multipleMasterData}")
    private String MULTIPLE_MASTER_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.userData}")
    private String USER_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.gridColorCodeData}")
    private String GRID_COLOR_CODE_DATA_URL;

    @Value("${v1service.url.base}${v1service.url.gridColorCodeCreate}")
    private String GRID_COLOR_CODE_CREATE_URL;

    @Value("${v1service.url.base}${v1service.url.gridColorCodeUpdate}")
    private String GRID_COLOR_CODE_UPDATE_URL;

    @Value("${v1service.url.base}${v1service.url.listCousinBranch}")
    private String LIST_COUSIN_BRANCH_URL;

    @Value("${v1service.url.base}${v1service.url.ListCousinBranchWithoutCurrent}")
    private String LIST_COUSIN_BRANCH_WITHOUT_CURRENT_URL;

    @Value("${v1service.url.base}${v1service.url.TenantIdByGuid}")
    private String TENANT_BY_GUID_URL;

    @Value("${v1service.url.base}${v1service.url.sendConsolidationTask}")
    private String SEND_CONSOLIDATION_TASK_URL;

    @Value("${v1service.url.base}${v1service.url.sendShipmentTask}")
    private String SEND_SHIPMENT_TASK_URL;

    @Value("${v1service.url.base}${v1service.url.importFlightSchedules}")
    private String IMPORT_FLIGHT_SCHEDULE;

    @Value("${v1service.url.base}${v1service.url.fetchFlightStatus}")
    private String FETCH_FLIGHT_STATUS;

    @Value("${v1service.url.base}${v1service.url.importSailingSchedules}")
    private String IMPORT_SAILING_SCHEDULES;

    @Value("${v1service.url.base}${v1service.url.listSailingSchedule}")
    private String LIST_SAILING_SCHEDULE;

    @Value("${v1service.url.base}${v1service.url.addressList}")
    private String ADDRESS_LIST;

    @Value("${v1service.url.base}${v1service.url.tenantNameByTenantId}")
    private String TENANT_NAME_BY_ID;

    @Value("${v1service.url.base}${v1service.url.chargeType}")
    private String CHARGE_TYPE_URL;

    @Value("${v1service.url.base}${v1service.url.unlocationOriginAndDestinationList}")
    private String UNLOCATION_ORIGIN_AND_DESTINATION_LIST_URL;

    @Value("${v1service.url.base}${v1service.url.listUnlocationTransportModeBased}")
    private String LIST_UNLOCATION_TRANSPORT_MODE_BASED_URL;

    @Value("${v1service.url.base}${v1service.url.activityMaster}")
    private String ACTIVITY_MASTER_URL;

    @Autowired
    private JsonHelper jsonHelper;
    @Override
    public ResponseEntity<?> createBooking(CustomerBooking customerBooking) {
        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(createBookingRequestForV1(customerBooking), V1AuthHelper.getHeaders());
            return this.restTemplate.postForEntity(this.CUSTOMER_BOOKING_URL, entity, V1ShipmentCreationResponse.class, new Object[0]);
        } catch (Exception exception) {
            var message = ((HttpServerErrorException.InternalServerError) exception).getResponseBodyAsString();
            throw new V1ServiceException(jsonHelper.readFromJson(message, V1ErrorResponse.class).getError().getMessage());
        }
    }

    @Override
    public V1DataResponse fetchMasterData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.MASTER_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getMasterData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createMasterData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.MASTER_DATA_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createMasterData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateMasterData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.MASTER_DATA_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateMasterData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchCarrierMasterData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            CarrierListObject req = jsonHelper.convertValue(request, CarrierListObject.class);
            Object requestCriteria = req.getListObject();
            HttpEntity<V1DataResponse> entity = new HttpEntity(requestCriteria, V1AuthHelper.getHeaders());
            if(req.getListObject() != null && req.getType() != null && (req.getType().equals(Constants.CONSOLIDATION_TYPE_AGT) || req.getType().equals(Constants.CONSOLIDATION_TYPE_CLD))) {
                masterDataResponse = this.restTemplate.postForEntity(this.CARRIER_MASTER_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            }
            else {
                masterDataResponse = this.restTemplate.postForEntity(this.CARRIER_MASTER_DATA_ORG_REF_FILTER_URL, entity, V1DataResponse.class, new Object[0]);
            }
            log.info("Token time taken in getCarrierMasterData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createCarrierMasterData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.CARRIER_MASTER_DATA_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createCarrierMasterData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateCarrierMasterData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.CARRIER_MASTER_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateCarrierMasterData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchContainerTypeData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.CONTAINER_TYPE_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getContainerTypeMasterData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createContainerTypeData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.CONTAINER_TYPE_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createContainerTypeData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateContainerTypeData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.CONTAINER_TYPE_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateContainerTypeData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchVesselData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.VESSEL_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getVesselData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createVesselData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.VESSEL_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createVesselData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateVesselData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.VESSEL_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateVesselData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchRoutingMasterData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.ROUTING_MASTER_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getRoutingMasterData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createRoutingMasterData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.ROUTING_MASTER_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createRoutingMasterData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateRoutingMasterData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.ROUTING_MASTER_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateRoutingMasterData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchCurrenciesData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.CURRENCIES_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getCurrenciesData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createCurrenciesData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.CURRENCIES_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createCurrenciesData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateCurrenciesData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.CURRENCIES_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateCurrenciesData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchDangerousGoodData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.DANGEROUS_GOOD_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getDangerousGoodData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createDangerousGoodData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.DANGEROUS_GOOD_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createDangerousGoodData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateDangerousGoodData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.DANGEROUS_GOOD_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateDangerousGoodData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchWarehouseData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.WAREHOUSE_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getWarehouseData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createWarehouseData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.WAREHOUSE_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createWarehouseData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateWarehouseData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.WAREHOUSE_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateWarehouseData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchPortsData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.PORTS_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getPortsData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createPortsData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.PORTS_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createPortsData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updatePortsData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.PORTS_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updatePortsData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchCommodityData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.COMMODITY_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getCommodityData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createCommodityData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.COMMODITY_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createCommodityData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateCommodityData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.COMMODITY_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateCommodityData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchSalesAgentData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.SALES_AGENT_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getSalesAgentData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createSalesAgentData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.SALES_AGENT_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createSalesAgentData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateSalesAgentData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.SALES_AGENT_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateSalesAgentData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createOrganizationData(Object request) {
        ResponseEntity orgResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            orgResponse = this.restTemplate.postForEntity(this.ORGANIZATION_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createOrganization() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) orgResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchOrganization(Object request) {
        ResponseEntity orgResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            orgResponse = this.restTemplate.postForEntity(this.ORGANIZATION_API, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getOrganization() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) orgResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateOrganizationData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.ORGANIZATION_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateOrganizationData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createUnlocationData(Object request) {
        ResponseEntity orgResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            orgResponse = this.restTemplate.postForEntity(this.UNLOCATION_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createUnlocation() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) orgResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchUnlocation(Object request) {
        ResponseEntity locationResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            locationResponse = this.restTemplate.postForEntity(this.UNLOCATION_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in getMasterData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) locationResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateUnlocationData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.UNLOCATION_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateUnlocationData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchMultipleMasterData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.MULTIPLE_MASTER_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in fetchMultipleMasterData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchUsersData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.USER_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in fetchUsersData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchGridColorCodeData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.GRID_COLOR_CODE_DATA_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in fetchGridColorCodeData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse createGridColorCodeData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.GRID_COLOR_CODE_CREATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in createGridColorCodeData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse updateGridColorCodeData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.GRID_COLOR_CODE_UPDATE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in updateGridColorCodeData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse listCousinBranches(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.LIST_COUSIN_BRANCH_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in listCousinBranches() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse listCousinBranchesWithoutCurrent(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.LIST_COUSIN_BRANCH_WITHOUT_CURRENT_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in listCousinBranchesWithoutCurrent() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public TenantIdResponse tenantByGuid(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.TENANT_BY_GUID_URL, entity, TenantIdResponse.class, new Object[0]);
            log.info("Token time taken in tenantByGuid() function " + (System.currentTimeMillis() - time));
            return (TenantIdResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public SendEntityResponse sendConsolidationTask(CreateConsolidationTaskRequest request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.SEND_CONSOLIDATION_TASK_URL, entity, SendEntityResponse.class, new Object[0]);
            log.info("Token time taken in sendConsolidationTask() function " + (System.currentTimeMillis() - time));
            return (SendEntityResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public SendEntityResponse sendShipmentTask(CreateShipmentTaskRequest request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.SEND_SHIPMENT_TASK_URL, entity, SendEntityResponse.class, new Object[0]);
            log.info("Token time taken in sendShipmentTask() function " + (System.currentTimeMillis() - time));
            return (SendEntityResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse importFlightSchedules(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.IMPORT_FLIGHT_SCHEDULE, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in importFlightSchedules() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchFlightStatus(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.FETCH_FLIGHT_STATUS, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in fetchFlightStatus() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse importSailingSchedules(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.IMPORT_SAILING_SCHEDULES, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in importSailingSchedules() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse listSailingSchedule(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.LIST_SAILING_SCHEDULE, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in listSailingSchedule() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse addressList(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.ADDRESS_LIST, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in addressList() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse tenantNameByTenantId(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.TENANT_NAME_BY_ID, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in tenantNameByTenantId() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }


    @Override
    public V1DataResponse fetchChargeCodeData(Object request) {
        ResponseEntity masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<V1DataResponse> entity = new HttpEntity(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.CHARGE_TYPE_URL, entity, V1DataResponse.class, new Object[0]);
            log.info("Token time taken in fetchChargeCodeData() function " + (System.currentTimeMillis() - time));
            return (V1DataResponse) masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchUnlocationOriginAndDestinationList(Object request) {
        ResponseEntity<V1DataResponse> masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<Object> entity = new HttpEntity<>(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.UNLOCATION_ORIGIN_AND_DESTINATION_LIST_URL, entity, V1DataResponse.class);
            log.info("Token time taken in fetchUnlocationOriginAndDestinationList() function " + (System.currentTimeMillis() - time));
            return masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchListUnlocationTransportModeBased(Object request) {
        ResponseEntity<V1DataResponse> masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<Object> entity = new HttpEntity<>(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.LIST_UNLOCATION_TRANSPORT_MODE_BASED_URL, entity, V1DataResponse.class);
            log.info("Token time taken in fetchListUnlocationTransportModeBased() function " + (System.currentTimeMillis() - time));
            return masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }

    @Override
    public V1DataResponse fetchActivityMaster(Object request) {
        ResponseEntity<V1DataResponse> masterDataResponse = null;

        try {
            long time = System.currentTimeMillis();
            HttpEntity<Object> entity = new HttpEntity<>(request, V1AuthHelper.getHeaders());
            masterDataResponse = this.restTemplate.postForEntity(this.ACTIVITY_MASTER_URL, entity, V1DataResponse.class);
            log.info("Token time taken in fetchActivityMaster() function " + (System.currentTimeMillis() - time));
            return masterDataResponse.getBody();
        } catch (HttpStatusCodeException var6) {
            if (var6.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                throw new UnAuthorizedException("UnAuthorizedException");
            } else {
                throw new V1ServiceException(var6.getMessage());
            }
        } catch (Exception var7) {
            throw new V1ServiceException(var7.getMessage());
        }
    }
}
