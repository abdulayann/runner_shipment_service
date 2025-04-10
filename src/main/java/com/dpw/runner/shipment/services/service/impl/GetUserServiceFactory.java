package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.service.interfaces.IUserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import static com.dpw.runner.shipment.services.commons.constants.Constants.USER_SERVICE_MAVANI;
import static com.dpw.runner.shipment.services.commons.constants.Constants.USER_SERVICE_V1;

@Configuration
public class GetUserServiceFactory {

    @Value("${user-service.source}")
    private String source;

    @Autowired
    UserServiceMavani userServiceMavani;

    @Autowired
    UserServiceV1 userServiceV1;

    public IUserService returnUserService()
    {
        if(source == null || source.isEmpty())
            return userServiceV1;  // LATER- throw exception?
        switch (source)
        {
            case USER_SERVICE_V1:
                return userServiceV1;
            case USER_SERVICE_MAVANI:
                return userServiceMavani;
            default:
                return userServiceV1;  // LATER- throw exception?
        }
    }
}
