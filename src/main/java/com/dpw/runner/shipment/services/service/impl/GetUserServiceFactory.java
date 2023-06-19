package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.service.interfaces.IUserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

@Configuration
public class GetUserServiceFactory {

    @Value("${user-service.source}")
    private String source;

    @Autowired
    UserService userService;

    @Autowired
    UserServiceV1 userServiceV1;

    public IUserService returnUserService()
    {
        if(source == null || source.isEmpty())
            return userServiceV1;  // throw exception
        switch (source)
        {
            case "v1":
                return userServiceV1;
            case "mavani":
                return userService;
            default:
                return userServiceV1;  // throw exception
        }
    }
}
