package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.service.interfaces.IUserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

import static com.dpw.runner.shipment.services.commons.constants.Constants.USER_SERVICE_MAVANI;
import static com.dpw.runner.shipment.services.commons.constants.Constants.USER_SERVICE_V1;

@Configuration
public class GetUserServiceFactory {

    @Autowired
    UserServiceMavani userServiceMavani;
    @Autowired
    UserServiceV1 userServiceV1;
    @Value("${user-service.source}")
    private String source;

    public IUserService returnUserService() {
        if (source == null || source.isEmpty())
            return userServiceV1;  // TODO- throw exception?
        switch (source) {
            case USER_SERVICE_V1:
                return userServiceV1;
            case USER_SERVICE_MAVANI:
                return userServiceMavani;
            default:
                return userServiceV1;  // TODO- throw exception?
        }
    }
}
